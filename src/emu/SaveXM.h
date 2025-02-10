#pragma once

class SaveXM
{
public:
	SaveXM();
	virtual ~SaveXM();

	size_t mSampleAddressOffset = 0;

	void sampleDataResize(size_t newSize);
	s8 getSampleFromAddress(u32 address);
	bool getSampleUsedFromAddress(u32 address);
	size_t getSamplesSize(void);
	void setSignedSampleForAddress(u32 address , s8 sample);

	bool saveSamples = true;
};
