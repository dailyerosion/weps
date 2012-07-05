// simple_gui.h : main header file for the simple_gui application
//
#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"       // main symbols


// Csimple_guiApp:
// See simple_gui.cpp for the implementation of this class
//

class Csimple_guiApp : public CWinApp
{
public:
	Csimple_guiApp();


// Overrides
public:
	virtual BOOL InitInstance();

// Implementation
	afx_msg void OnAppAbout();
	DECLARE_MESSAGE_MAP()
};

extern Csimple_guiApp theApp;