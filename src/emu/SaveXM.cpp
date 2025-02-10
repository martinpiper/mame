#include "emu.h"
#include "SaveXM.h"

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
		return 0;
	}
	return sSamplesUsed[address];
}

size_t SaveXM::getSamplesSize(void)
{
	return sSamplesUsed.size();
}

void SaveXM::setSignedSampleForAddress(u32 address, s8 sample)
{
	sSamples[address] = sample;
	sSamplesUsed[address] = true;
}

