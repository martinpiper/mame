#pragma once

class SaveXM
{
public:
	SaveXM();
	virtual ~SaveXM();

	size_t mSampleAddressOffset = 0;
	bool saveSamples = true;

	void sampleDataResize(size_t newSize);
	s8 getSampleFromAddress(u32 address);
	bool getSampleUsedFromAddress(u32 address);
	size_t getSamplesSize(void);
	void setSignedSampleForAddress(u32 address , s8 sample);

	void resizeAnyNotesInChannel(size_t newSize);

	void setPitch(double theTime , int theChannel , double pitchForStep);

	void setNoteOn(double theTime, int theChannel, u32 start, u32 end, u32 loopStart, u32 loopEnd, double pitchForStep, int volume, int pan);
	void setNoteRelease(double theTime, int theChannel);
	void setNoteOff(double theTime, int theChannel);
	void setVolume(double theTime, int theChannel, int volume);

	void writeXMFile(void);
};
