#pragma once

class WepsRunFile
{
public:
	WepsRunFile(const char *file);
	~WepsRunFile(void);
	const char *getClimateFile() { return climateFile;};
	const char *getWindFile() { return windFile; };
	const char *getIFCFile() { return ifcFile;};
	const char *getManageFile() { return manageFile;};
private:
	char climateFile[256];
	char windFile[256];
	char ifcFile[256];
	char manageFile[256];
};
