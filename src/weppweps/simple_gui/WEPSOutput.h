#pragma once

class WEPSOutput
{
public:
	WEPSOutput(const char *file);
	const char *getTotalAsString();
	~WEPSOutput(void);
private:
	char total[32];
};
