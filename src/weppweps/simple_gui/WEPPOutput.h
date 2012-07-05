#pragma once

class WEPPOutput
{
public:
	WEPPOutput(const char *file);
	const char *getTotalAsString();
	~WEPPOutput(void);
private:
	char total[32];
};
