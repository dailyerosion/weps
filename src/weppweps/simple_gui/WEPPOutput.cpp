#include "StdAfx.h"
#include "WEPPOutput.h"

WEPPOutput::WEPPOutput(const char *file)
{
	char buf[256];
	FILE *fp = fopen(file,"r");
	while (fgets(buf,255,fp)) {
		if (strstr(buf,"soil loss (avg. of net detachment areas)") != 0) {
			char *p = strstr(buf,"=");
			if (p) {
				p++;
				while ((*p == ' ') || (*p == '\t')) p++;
				int i = 0;
				while ((*p != '\0') && (*p != '\n') && (*p != '*')) { total[i++] = *p; p++; }
				total[i] = '\0';
				strcat(total,"/year");
			}

		}
	}
	fclose(fp);
}

WEPPOutput::~WEPPOutput(void)
{
}

const char *WEPPOutput::getTotalAsString()
{
	return total;
}