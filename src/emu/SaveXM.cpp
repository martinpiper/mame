#include "emu.h"
#include "SaveXM.h"
#include <map>
#include <utility>

// Trying to merge all intances into one output
static std::vector<s8> sSamples;
static std::vector<bool> sSamplesUsed;

SaveXM::SaveXM()
{
}

SaveXM::~SaveXM()
{
}

void SaveXM::sampleDataResize(size_t newSize)
{
	sSamples.resize(newSize, 0);
	sSamplesUsed.resize(newSize, false);
}

s8 SaveXM::getSampleFromAddress(u32 address)
{
	if (address >= sSamples.size())
	{
		return 0;
	}
	return sSamples[address];
}
bool SaveXM::getSampleUsedFromAddress(u32 address)
{
	if (address >= sSamples.size())
	{
		return true;
	}
	return sSamplesUsed[address];
}

size_t SaveXM::getSamplesSize(void)
{
	return sSamplesUsed.size();
}

void SaveXM::setSignedSampleForAddress(u32 address, s8 sample)
{
	if (address < sSamples.size())
	{
		sSamples[address] = sample;
		sSamplesUsed[address] = true;
	}
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
		if (mByte & 0x08)
		{
			writeByte(mEffect);
		}
		if (mByte & 0x10)
		{
			writeByte(mEffectParameter);
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

		mInstrument = (u8)(sampleIndex + 1);

		return *this;
	}

	SoundEvent& SetVolume(int volume)
	{
		mByte |= 0x04;

		mVolume = (u8)((0x40 * (0x7f - volume)) / 0x7f);

		return *this;
	}

	SoundEvent& SetPan(int pan)
	{
		mByte |= 0x18;

		mEffect = 0x08;
		mEffectParameter = 8 + (pan << 4);

		return *this;
	}

	u8 mByte = 0x80;
	u8 mNote = 0;
	u8 mInstrument = 0;
	u8 mVolume = 0;
	u8 mEffect = 0;
	u8 mEffectParameter = 0;
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
static bool sWriteData = true;
std::map< PotentialSample, int> sPotentialSampleToIndex;
static std::vector<bool> sAnyNotesinChannel;
static std::vector< std::vector< SoundEvent* > > sMusicRows;
double sFirstEventDelta = -1.0f;

void SaveXM::resizeAnyNotesInChannel(size_t newSize)
{
	sAnyNotesinChannel.resize(newSize);
}

static std::vector< SoundEvent* >&getRowForTime(double theTime)
{
	theTime = theTime - sFirstEventDelta;
	//			int theRowIndex = (int)(theTime * 100.0f);	// 250 BPM
	int theRowIndex = (int)(theTime * 50.0f);	// 125 BPM

	sMusicRows.resize(theRowIndex + 1);
	std::vector< SoundEvent* >& theRow = sMusicRows[theRowIndex];
	theRow.resize(sAnyNotesinChannel.size());	// Ensures we have enough channels

	return theRow;
}

void SaveXM::setPitch(double theTime , int theChannel , double pitchForStep)
{
	std::vector< SoundEvent* >& theRow = getRowForTime(theTime);

	if (theChannel < theRow.size())
	{
		SoundEvent* event = theRow[theChannel];
		if (!event)
		{
			event = new SoundEvent();
			theRow[theChannel] = event;
		}
		event->SetNote(pitchForStep);
		sAnyNotesinChannel[theChannel] = true;
	}
}

void SaveXM::setNoteOn(double theTime, int theChannel, u32 start, u32 end, u32 loopStart, u32 loopEnd, double pitchForStep, int volume, int pan)
{
	if (sFirstEventDelta < 0.0f)
	{
		sFirstEventDelta = theTime;
	}

	std::vector< SoundEvent* >& theRow = getRowForTime(theTime);

	PotentialSample potentialSample(start, end, loopStart, loopEnd);
	auto retInsert = sPotentialSampleToIndex.insert(std::pair< PotentialSample, int>(potentialSample, (int)sPotentialSampleToIndex.size()));
	int sampleIndex = retInsert.first->second;

	if (theChannel < theRow.size())
	{
		SoundEvent* event = theRow[theChannel];
		if (!event)
		{
			event = new SoundEvent();
			theRow[theChannel] = event;
		}
		event->SetSampleIndex(sampleIndex).SetNote(pitchForStep).SetVolume(volume);
		event->SetPan(pan);
		sAnyNotesinChannel[theChannel] = true;
	}

}

void SaveXM::setNoteRelease(double theTime, int theChannel)
{
	std::vector< SoundEvent* >& theRow = getRowForTime(theTime);

	SoundEventNoteRelease* event = new SoundEventNoteRelease();
	if (theChannel < theRow.size())
	{
		theRow[theChannel] = event;
		sAnyNotesinChannel[theChannel] = true;
	}
}

void SaveXM::setNoteOff(double theTime, int theChannel)
{
	std::vector< SoundEvent* >& theRow = getRowForTime(theTime);

	SoundEventNoteOff* event = new SoundEventNoteOff();
	if (theChannel < theRow.size())
	{
		theRow[theChannel] = event;
		sAnyNotesinChannel[theChannel] = true;
	}
}

void SaveXM::setVolume(double theTime, int theChannel, int volume)
{
	std::vector< SoundEvent* >& theRow = getRowForTime(theTime);

	if (theChannel < theRow.size())
	{
		SoundEvent* event = theRow[theChannel];
		if (!event)
		{
			event = new SoundEvent();
			theRow[theChannel] = event;
		}
		event->SetVolume(volume);
	}
}

void SaveXM::writeXMFile(void)
{
	// Guards for no data and already written. This is because MAME has a habbit if instantiating parent classes without runnign the machines.
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
			totalUsedSamples++;
		}
	}

	printf("totalUsedSamples $%x (%d)\n", totalUsedSamples, totalUsedSamples);
	printf("used rows $%x (%d)\n", (int)sMusicRows.size(), (int)sMusicRows.size());
	for (size_t i = 0; i < sAnyNotesinChannel.size(); i++)
	{
		printf("channel %d : %d\n", (int)i, (int)sAnyNotesinChannel[i]);
	}

	FILE* fp = fopen("c:\\temp\\SamplesUsed.bin", "wb");
	for (u32 i = 0; i < getSamplesSize(); i++)
	{
		if (getSampleUsedFromAddress(i))
		{
			s8 sampleByte = getSampleFromAddress(i);
			fputc(sampleByte, fp);
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
						if ((channel + 32) < theRow.size())
						{
							soundEvent = theRow[channel + 32];
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
					s8 sampleByte = getSampleFromAddress(i);
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
