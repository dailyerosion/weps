#include "StdAfx.h"
#include "WepsRunFile.h"

WepsRunFile::WepsRunFile(const char *file)
{
	char buf[512];

	FILE *fp = fopen(file,"r");
	if (fp) {
		while (fgets(buf,256,fp)) {
			if (strstr(buf,"RFD-CliGenStationName")) {
				
			} else if (strstr(buf,"RFD-WindGenStationName")) {

			} else if (strstr(buf,"RFD-climate file")) {
				fgets(buf,256,fp);
				int i=0;
				while ((buf[i] != '\n') && (buf[i] != '\0'))
					climateFile[i] = buf[i++];
				climateFile[i] = '\0';
				
		        
			} else if (strstr(buf,"RFD-wind file")) {
				fgets(buf,256,fp);
				int i=0;
				while ((buf[i] != '\n') && (buf[i] != '\0'))
					windFile[i] = buf[i++];
				windFile[i] = '\0';
				
			} else if (strstr(buf,"RFD-SoilFile")) {
				fgets(buf,256,fp);
				int i=0;
				while ((buf[i] != '\n') && (buf[i] != '\0'))
					ifcFile[i] = buf[i++];
				ifcFile[i] = '\0';
				
			} else if (strstr(buf,"RFD-ManageFile")) {
				fgets(buf,256,fp);
				int i=0;
				while ((buf[i] != '\n') && (buf[i] != '\0'))
					manageFile[i] = buf[i++];
				manageFile[i] = '\0';
				
			} 

		}
		fclose(fp);
	}
}

WepsRunFile::~WepsRunFile(void)
{
}
