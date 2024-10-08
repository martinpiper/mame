// license:BSD-3-Clause
// copyright-holders:Miguel Angel Horna
/*
 * Yamaha YMW-258-F 'GEW8' (aka Sega 315-5560) emulation.
 *
 * by Miguel Angel Horna (ElSemi) for Model 2 Emulator and MAME.
 * Information by R. Belmont and the YMF278B (OPL4) manual.
 *
 * voice registers:
 * 0: Pan
 * 1: Index of sample
 * 2: LSB of pitch (low 2 bits seem unused so)
 * 3: MSB of pitch (ooooppppppppppxx) (o=octave (4 bit signed), p=pitch (10 bits), x=unused?
 * 4: voice control: top bit = 1 for key on, 0 for key off
 * 5: bit 0: 0: interpolate volume changes, 1: direct set volume,
 *    bits 1-7 = volume attenuate (0=max, 7f=min)
 * 6: LFO frequency + Phase LFO depth
 * 7: Amplitude LFO size
 *
 * The first sample ROM contains a variable length metadata table with 12
 * bytes per instrument sample. This is very similar to the YMF278B 'OPL4'.
 * This sample format might be derived from the one used by the older YM7138 'GEW6' chip.
 *
 * The first 3 bytes are the offset into the file (big endian). (0, 1, 2).
 * Bit 23 is unknown.
 * Bit 22 is the sample format flag: 0 for 8-bit linear, 1 for 12-bit linear.
 * Bit 21 is used by the MU5 on some samples for as-yet unknown purposes. (YMW-258-F has 22 address pins.)
 * The next 2 bytes are the loop start point, in samples (big endian) (3, 4)
 * The next 2 are the 2's complement negation of of the total number of samples (big endian) (5, 6)
 * The next byte is LFO freq + depth (copied to reg 6 ?) (7, 8)
 * The next 3 are envelope params (Attack, Decay1 and 2, sustain level, release, Key Rate Scaling) (9, 10, 11)
 * The next byte is Amplitude LFO size (copied to reg 7 ?)
 *
 * TODO
 * - http://dtech.lv/techarticles_yamaha_chips.html indicates FM support, which we don't have yet.
 */

#include "emu.h"
#include "multipcm.h"

const int32_t multipcm_device::VALUE_TO_CHANNEL[32] =
{
	0, 1, 2, 3, 4, 5, 6 , -1,
	7, 8, 9, 10,11,12,13, -1,
	14,15,16,17,18,19,20, -1,
	21,22,23,24,25,26,27, -1,
};

void multipcm_device::init_sample(sample_t &sample, uint32_t index)
{
	uint32_t address = index * 12;

	sample.m_start = (read_byte(address) << 16) | (read_byte(address + 1) << 8) | read_byte(address + 2);
	sample.m_format = (sample.m_start>>20) & 0xfe;
	sample.m_start &= 0x3fffff;
	sample.m_loop = (read_byte(address + 3) << 8) | read_byte(address + 4);
	sample.m_end = 0x10000 - ((read_byte(address + 5) << 8) | read_byte(address + 6));
	sample.m_attack_reg = (read_byte(address + 8) >> 4) & 0xf;
	sample.m_decay1_reg = read_byte(address + 8) & 0xf;
	sample.m_decay2_reg = read_byte(address + 9) & 0xf;
	sample.m_decay_level = (read_byte(address + 9) >> 4) & 0xf;
	sample.m_release_reg = read_byte(address + 10) & 0xf;
	sample.m_key_rate_scale = (read_byte(address + 10) >> 4) & 0xf;
	sample.m_lfo_vibrato_reg = read_byte(address + 7);
	sample.m_lfo_amplitude_reg = read_byte(address + 11) & 0xf;
}

class PotentialSample
{
public:
	PotentialSample(u32 start, u32 end, u32 loopStart, u32 loopEnd) : mStart(start), mEnd(end), mLoopStart(loopStart), mLoopEnd(loopEnd) {}
	virtual ~PotentialSample() {}

	u32 mStart;
	u32 mEnd;
	u32 mLoopStart;
	u32 mLoopEnd;

	friend bool operator<(const PotentialSample& lhs, const PotentialSample& rhs)
	{
		return std::tie(lhs.mStart, lhs.mEnd, lhs.mLoopStart, lhs.mLoopEnd) < std::tie(rhs.mStart, rhs.mEnd, rhs.mLoopStart, rhs.mLoopEnd);
	}
};

static FILE* sFP = 0;
static void writeByte(u8 byteValue)
{
	fwrite(&byteValue, 1, 1, sFP);

}
static void writeWord(u16 wordValue)
{
	fwrite(&wordValue, 1, 2, sFP);
}
static void writeLong(u32 longValue)
{
	fwrite(&longValue, 1, 4, sFP);
}

// TODO: It would be better to accumulate changes, note, instrument, volume, into one event.
// This would handle volume being changed in the cycles just after starting a note, and also pitch changes.
class SoundEvent
{
public:
	SoundEvent() {}
	virtual ~SoundEvent() {}

	virtual void Write(void)
	{
		writeByte(mByte);
		if (mByte & 0x01)
		{
			writeByte(mNote);
		}
		if (mByte & 0x02)
		{
			writeByte(mInstrument);
		}
		if (mByte & 0x04)
		{
			writeByte(mVolume);
		}
	}

	SoundEvent& SetNote(double theNote)
	{
		mByte |= 0x01;

		double midiNote = 12.0f * log2(theNote / 440.0f);
		int imidiNote = (int)(midiNote - 1);
		if (imidiNote < 0)
		{
			imidiNote = 0;
		}
		if (imidiNote > 96)
		{
			imidiNote = 96;
		}
		mNote = (u8)imidiNote;

		return *this;
	}

	SoundEvent& SetSampleIndex(int sampleIndex)
	{
		mByte |= 0x02;

		mInstrument = (u8) (sampleIndex + 1);

		return *this;
	}

	SoundEvent& SetVolume(int volume)
	{
		mByte |= 0x04;

		mVolume = (u8)((0x40 * (0x7f - volume)) / 0x7f);

		return *this;
	}

	u8 mByte = 0x80;
	u8 mNote = 0;
	u8 mInstrument = 0;
	u8 mVolume = 0;
};

class SoundEventNoteOff : public SoundEvent
{
public:
	SoundEventNoteOff()
	{
		SetVolume(0);
	}
	virtual ~SoundEventNoteOff() {}
};

class SoundEventNoteRelease : public SoundEvent
{
public:
	SoundEventNoteRelease()
	{
		mNote = 97;	// Note off
		mByte |= 0x01;
	}
	virtual ~SoundEventNoteRelease() {}
};


// Why static and not inside the multipcm_device class? Well this attempts to merge all instances of multipcm_device into one output
static int sInstanceChannelOffset = 0;
static bool sWriteData = true;
std::map< PotentialSample, int> sPotentialSampleToIndex;
static std::vector<bool> sAnyNotesinChannel;
static std::vector< std::vector< SoundEvent* > > sMusicRows;
double sFirstEventDelta = -1.0f;

void multipcm_device::write_slot(slot_t &slot, int32_t reg, uint8_t data)
{
	if (!mInstanceInit)
	{
		mInstanceInit = true;

		mChannelOffset = sInstanceChannelOffset;
		sInstanceChannelOffset += 32;
		sAnyNotesinChannel.resize(sInstanceChannelOffset);
	}
//	const address_space_config *memConfig = memory_space_config().front().second;
//	double theTime = m_stream->sample_time().as_double();
	m_stream->update();
	slot.m_regs[reg] = data;

	switch(reg)
	{
		case 0: // PANPOT
			slot.m_pan = (data >> 4) & 0xf;
			break;

		case 1: // Sample
		{
			// according to YMF278 sample write causes some base params written to the regs (envelope+lfos)
			init_sample(slot.m_sample, slot.m_regs[1] | ((slot.m_regs[2] & 1) << 8));

			write_slot(slot, 6, slot.m_sample.m_lfo_vibrato_reg);
			write_slot(slot, 7, slot.m_sample.m_lfo_amplitude_reg);

			// retrigger if key is on
			if (slot.m_playing)
			{
				retrigger_sample(slot);
			}

			break;
		}
		case 2: // Pitch
		case 3:
			{
				slot.m_octave = slot.m_regs[3] >> 4;
				slot.m_pitch = ((slot.m_regs[3] & 0xf) << 6) | (slot.m_regs[2] >> 2);
				update_step(slot);

				if (m_cur_slot >= 0 && slot.m_playing)
				{
					double theTime = m_stream->sample_time().as_double();

					theTime = theTime - sFirstEventDelta;
					//			int theRowIndex = (int)(theTime * 100.0f);	// 250 BPM
					int theRowIndex = (int)(theTime * 50.0f);	// 125 BPM

					sMusicRows.resize(theRowIndex + 1);
					std::vector< SoundEvent* >& theRow = sMusicRows[theRowIndex];
					theRow.resize(sInstanceChannelOffset);	// Ensures we have enough channels

					int theChannel = mChannelOffset + m_cur_slot;
					if (theChannel < theRow.size())
					{
						SoundEvent* event = theRow[theChannel];
						if (!event)
						{
							event = new SoundEvent();
							theRow[theChannel] = event;
						}
						event->SetNote(slot.m_pitchForStep);
						sAnyNotesinChannel[theChannel] = true;
					}
				}
			}
			break;
		case 4: // KeyOn/Off
		{
			double theTime = m_stream->sample_time().as_double();
			if (data & 0x80) // Only do this on the first real KeyOn
			{
				if (sFirstEventDelta < 0.0f)
				{
					sFirstEventDelta = theTime;
				}
			}
			// Make the first event appear at the start of the output file :)
			theTime = theTime - sFirstEventDelta;
//			int theRowIndex = (int)(theTime * 100.0f);	// 250 BPM
			int theRowIndex = (int)(theTime * 50.0f);	// 125 BPM

			sMusicRows.resize(theRowIndex + 1);
			std::vector< SoundEvent* >& theRow = sMusicRows[theRowIndex];
			theRow.resize(sInstanceChannelOffset);	// Ensures we have enough channels
			// m_cur_slot

			if (data & 0x80) // KeyOn
			{
				slot.m_playing = true;
				retrigger_sample(slot);

				if (m_cur_slot >= 0)
				{
					PotentialSample potentialSample(mSampleAddressOffset + slot.m_sample.m_start, mSampleAddressOffset + slot.m_sample.m_start + slot.m_sample.m_end, mSampleAddressOffset + slot.m_sample.m_start + slot.m_sample.m_loop, mSampleAddressOffset + slot.m_sample.m_start + slot.m_sample.m_end);
					auto retInsert = sPotentialSampleToIndex.insert(std::pair< PotentialSample, int>(potentialSample, (int)sPotentialSampleToIndex.size()));
					slot.mXMSampleIndex = retInsert.first->second;

					int theChannel = mChannelOffset + m_cur_slot;
					if (theChannel < theRow.size())
					{
						SoundEvent* event = theRow[theChannel];
						if (!event)
						{
							event = new SoundEvent();
							theRow[theChannel] = event;
						}
						event->SetSampleIndex(slot.mXMSampleIndex).SetNote(slot.m_pitchForStep).SetVolume(slot.m_dest_total_level);
						sAnyNotesinChannel[theChannel] = true;
					}
				}
			}
			else
			{
				if (slot.m_playing)
				{
					if (slot.m_sample.m_release_reg != 0xf)
					{
						slot.m_envelope_gen.m_state = state_t::RELEASE;

						SoundEventNoteRelease* event = new SoundEventNoteRelease();
						if (m_cur_slot >= 0)
						{
							int theChannel = mChannelOffset + m_cur_slot;
							if (theChannel < theRow.size())
							{
								theRow[theChannel] = event;
								sAnyNotesinChannel[theChannel] = true;
							}
						}

					}
					else
					{
						slot.m_playing = false;

						SoundEventNoteOff* event = new SoundEventNoteOff();
						if (m_cur_slot >= 0)
						{
							int theChannel = mChannelOffset + m_cur_slot;
							if (theChannel < theRow.size())
							{
								theRow[theChannel] = event;
								sAnyNotesinChannel[theChannel] = true;
							}
						}
					}
				}
			}
		}
		break;
		case 5: // TL + Interpolation
			slot.m_dest_total_level = (data >> 1) & 0x7f;
			if (!(data & 1)) // Interpolate TL
			{
				if ((slot.m_total_level >> TL_SHIFT) > slot.m_dest_total_level)
				{
					slot.m_total_level_step = m_total_level_steps[0]; // decrease
				}
				else
				{
					slot.m_total_level_step = m_total_level_steps[1]; // increase
				}
			}
			else
			{
				slot.m_total_level = slot.m_dest_total_level << TL_SHIFT;
			}
			if (m_cur_slot >= 0 && slot.m_playing)
			{
				double theTime = m_stream->sample_time().as_double();

				theTime = theTime - sFirstEventDelta;
				//			int theRowIndex = (int)(theTime * 100.0f);	// 250 BPM
				int theRowIndex = (int)(theTime * 50.0f);	// 125 BPM

				sMusicRows.resize(theRowIndex + 1);
				std::vector< SoundEvent* >& theRow = sMusicRows[theRowIndex];
				theRow.resize(sInstanceChannelOffset);	// Ensures we have enough channels

				int theChannel = mChannelOffset + m_cur_slot;
				if (theChannel < theRow.size())
				{
					SoundEvent* event = theRow[theChannel];
					if (!event)
					{
						event = new SoundEvent();
						theRow[theChannel] = event;
					}
					event->SetVolume(slot.m_dest_total_level);
				}
			}
			break;
		case 6: // LFO frequency + Pitch LFO
		case 7: // Amplitude LFO
			slot.m_lfo_frequency = (slot.m_regs[6] >> 3) & 7;
			slot.m_vibrato = slot.m_regs[6] & 7;
			slot.m_tremolo = slot.m_regs[7] & 7;
			if (data)
			{
				lfo_compute_step(slot.m_pitch_lfo, slot.m_lfo_frequency, slot.m_vibrato, 0);
				lfo_compute_step(slot.m_amplitude_lfo, slot.m_lfo_frequency, slot.m_tremolo, 1);
			}
			break;
	}
}

uint8_t multipcm_device::read()
{
	return 0;
}

void multipcm_device::write(offs_t offset, uint8_t data)
{
	switch(offset)
	{
		case 0: // Data write
			write_slot(m_slots[m_cur_slot], m_address, data);
			break;
		case 1:
			m_cur_slot = VALUE_TO_CHANNEL[data & 0x1f];
			break;

		case 2:
			m_address = (data > 7) ? 7 : data;
			break;
	}
}


/* MAME access functions */

DEFINE_DEVICE_TYPE(MULTIPCM, multipcm_device, "ymw258f", "Yamaha YMW-258-F")

multipcm_device::multipcm_device(const machine_config &mconfig, const char *tag, device_t *owner, uint32_t clock) :
	gew_pcm_device(mconfig, MULTIPCM, tag, owner, clock, 28, 224),
	m_cur_slot(0),
	m_address(0),
	mInstanceInit(false)
{
}

multipcm_device::~multipcm_device()
{
	if (getSamplesSize() == 0)
	{
		return;
	}
	if (!sWriteData)
	{
		return;
	}

	sWriteData = false;

	int totalUsedSamples = 0;

	bool sLastState = false;
	for (size_t i = 0; i < getSamplesSize(); i++)
	{
		if (sLastState != getSampleUsedFromAddress(i))
		{
			if (getSampleUsedFromAddress(i))
			{
				printf("Start sample: $%x\n", (int)i);
			}
			else
			{
				printf("End sample: $%x\n", (int)i);
			}
			sLastState = getSampleUsedFromAddress(i);
		}
		if (getSampleUsedFromAddress(i))
		{
			totalUsedSamples ++;
		}
	}

	printf("totalUsedSamples $%x (%d)\n", totalUsedSamples, totalUsedSamples);
	printf("used rows $%x (%d)\n", (int)sMusicRows.size(), (int)sMusicRows.size());
	for (size_t i = 0; i < sAnyNotesinChannel.size(); i++)
	{
		printf("channel %d : %d\n", (int)i, (int)sAnyNotesinChannel[i]);
	}

	FILE *fp = fopen("c:\\temp\\SamplesUsed.bin", "wb");
	for (u32 i = 0; i < getSamplesSize(); i++)
	{
		if (getSampleUsedFromAddress(i))
		{
			s8 sampleByte = (s8)getSampleFromAddress(i);
			fputc(sampleByte , fp);
		}
	}
	fclose(fp);

	sFP = fopen("c:\\temp\\t.xm", "wb");

	fwrite("Extended Module:     ", 1, 17, sFP);
	fwrite("Some name which is a certain length", 1, 20, sFP);
	writeByte(0x1a);
	fwrite("FastTracker v2.00    ", 1, 20, sFP);
	writeWord(0x104);
	writeLong(0x114);

	// Song length
	writeWord(256);
	// Restart position
	writeWord(0x00);
	// Number of channels
	writeWord(32);
	// Number of patterns
	writeWord(256);
	// Number of instruments
	writeWord(sPotentialSampleToIndex.size());
	// Flags
	writeWord(0x01);	// Specifies a **linear** frequency table
	// Tempo
	//writeWord(0x05);
	writeWord(1);
	// BPM
	// Daytona (adv) = 49 seconds from the start to the start of the final "ahhhhhhhh"...
//	writeWord(250);	// Works with * 100.0f
	writeWord(125);	// Works with * 50.0f
	//writeWord(50 * 60);
	// Pattern order table
	for (int i = 0; i < 256; i++)
	{
		writeByte(i);
	}

	auto rowIterator = sMusicRows.begin();
	for (int pattern = 0; pattern < 256; pattern++)
	{
		// Pattern header length
		writeLong(0x09);
		// Packing type
		writeByte(0x00);
		// Number of rows
		writeWord(256);
		// Number of bytes for pattern data
		int sizePos = ftell(sFP);
		writeWord(256 * 64);	// Which is going to be wrong at this stage
		int startPos = ftell(sFP);
		for (int row = 0; row < 256; row++)
		{
			if (rowIterator != sMusicRows.end())
			{
				auto theRow = *rowIterator;
				rowIterator++;

				for (int channel = 0; channel < 32; channel++)
				{
					SoundEvent* soundEvent = 0;

					if (channel < theRow.size())
					{
						soundEvent = theRow[channel];
					}

					// Try to merge the other 32 channels from the emulation into the first 32 XM channels
					if (!soundEvent)
					{
						if ((channel+32) < theRow.size())
						{
							soundEvent = theRow[channel+32];
						}
					}
					
					if (soundEvent)
					{
						soundEvent->Write();
					}
					else
					{
						writeByte(0x80);	// Empty
					}

				}
			}
			else
			{
				for (int channel = 0; channel < 32; channel++)
				{
					writeByte(0x80);	// Empty
				}
			}
		}
		int endPos = ftell(sFP);
		// Write the real size of the pattern
		fseek(sFP, sizePos, SEEK_SET);
		writeWord(endPos - startPos);
		fseek(sFP, endPos, SEEK_SET);
	}

	// Now instruments, in order of their sample index, not the map order
	for (int desiredSampleIndex = 0; desiredSampleIndex < (int)sPotentialSampleToIndex.size(); desiredSampleIndex++)
	{
		bool wroteOne = false;
		auto sampleIterator = sPotentialSampleToIndex.begin();
		while (sampleIterator != sPotentialSampleToIndex.end())
		{
			if (sampleIterator->second == desiredSampleIndex)
			{
				writeLong(0x107);
				fwrite("Instrument\0            ", 1, 22, sFP);
				writeByte(0); // Type
				writeWord(1); // One sample for this instrument
				// And sample
				writeLong(0x28);	// Size
				// Keymap, volume envelope, panning envelope
				for (int i = 0; i < 96 + 48 + 48; i++)
				{
					writeByte(0);
				}
				// Envelope points...
				for (int i = 0; i < 14; i++)
				{
					writeByte(0);
				}
				// Volume fadeout
				writeWord(1);
				// Reserved
				for (int i = 0; i < 22; i++)
				{
					writeByte(0);
				}
				// Sample header
				// Sample length
				writeLong(sampleIterator->first.mEnd - sampleIterator->first.mStart);
				// Sample loop start
				writeLong(sampleIterator->first.mLoopStart - sampleIterator->first.mStart);
				// Sample loop length
				writeLong(sampleIterator->first.mLoopEnd - sampleIterator->first.mLoopStart);
				// Volume
				writeByte(0x40);
				// Finetune
				writeByte(0);
				// Type, looping...
				writeByte(0x01);
				// Panning
				writeByte(0x80);
				// Relative note number
				writeByte(0x00);
				// Packing type
				writeByte(0x00);
				fwrite("Sample name\0           ", 1, 22, sFP);
				int oldSample = 0;
				for (u32 i = sampleIterator->first.mStart; i < sampleIterator->first.mEnd; i++)
				{
					s8 sampleByte = (s8)getSampleFromAddress(i);
					writeByte(sampleByte - oldSample);
					oldSample = sampleByte;
				}
				wroteOne = true;
				break;
			}
			sampleIterator++;
		}
		assert(wroteOne);
	}


	fclose(sFP);
}
