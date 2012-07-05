#include "StdAfx.h"
#include "WEPSOutput.h"

WEPSOutput::WEPSOutput(const char *file)
{
	char buf[256];
	FILE *fp = fopen(file,"r");
	while (fgets(buf,255,fp)) {
		if (strstr(buf,"Ave. Annual") != 0) {
			int j=0;
			for (int i=0;i<5;i++) {
               while (buf[j] != '|') j++;
			   j++;
			}
			int k=0;
			while ((buf[j] == ' ') || (buf[j] == '\t')) j++;
			while ((buf[j] != '\0') && (buf[j] != '|')) { total[k++] = buf[j++]; }
			total[k] = '\0';
			strcat(total," kg/m2/year");
		}
	}
	fclose(fp);
}

WEPSOutput::~WEPSOutput(void)
{
}

const char *WEPSOutput::getTotalAsString()
{
	return total;
}
