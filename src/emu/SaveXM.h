#pragma once

class SaveXM
{
public:
	SaveXM();
	virtual ~SaveXM();

	size_t mSampleAddressOffset = 0;
	bool saveSamples = true;

	// Look for these functions being used in other sound drivers (e.g. multipcm.cpp) and implement similar calling code in the sound device
	// These extract sample data
	void sampleDataResize(size_t newSize);
	void setSignedSampleForAddress(u32 address, s8 sample);
	void resizeAnyNotesInChannel(size_t newSize);

	// These extract note data
	void setNoteOn(double theTime, int theChannel, u32 start, u32 end, u32 loopStart, u32 loopEnd, double pitchForStep, int volume, int pan);
	void setNoteRelease(double theTime, int theChannel);
	void setNoteOff(double theTime, int theChannel);
	void setVolume(double theTime, int theChannel, int volume);
	void setPitch(double theTime, int theChannel, double pitchForStep);

	// Support functions, can be used when calling the above functions
	bool getSampleUsedFromAddress(u32 address);
	size_t getSamplesSize(void);

	// Used when writing data
	void writeXMFile(void);
	s8 getSampleFromAddress(u32 address);
};
