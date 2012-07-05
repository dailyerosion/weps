// simple_guiView.cpp : implementation of the Csimple_guiView class
//

#include "stdafx.h"
#include "simple_gui.h"

#include "simple_guiDoc.h"
#include "simple_guiView.h"
#include "wepsrunfile.h"
#include "WEPPOutput.h"
#include "WEPSOutput.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// Csimple_guiView

IMPLEMENT_DYNCREATE(Csimple_guiView, CFormView)

BEGIN_MESSAGE_MAP(Csimple_guiView, CFormView)
	ON_COMMAND(ID_FILE_OPEN, &Csimple_guiView::OnFileOpen)
//	ON_BN_CLICKED(IDC_BUTTON1, &Csimple_guiView::OnBnClickedButton1)
ON_BN_CLICKED(IDC_BUTTON1, &Csimple_guiView::onRun)
ON_BN_CLICKED(IDC_WEPS, &Csimple_guiView::OnMainWEPS)
ON_BN_CLICKED(IDC_WEPPMAIN, &Csimple_guiView::OnWEPP)
ON_BN_CLICKED(IDC_WEPPEVE, &Csimple_guiView::OnEvents)
ON_BN_CLICKED(IDC_PLOT, &Csimple_guiView::OnPlot)
ON_BN_CLICKED(IDC_RUNOFF, &Csimple_guiView::OnRunoff)
ON_BN_CLICKED(IDC_INRUN, &Csimple_guiView::OnInRun)
ON_BN_CLICKED(IDC_CLI, &Csimple_guiView::OnClimate)
ON_BN_CLICKED(IDC_WIND, &Csimple_guiView::OnWind)
ON_BN_CLICKED(IDC_MAN, &Csimple_guiView::OnMan)
ON_BN_CLICKED(IDC_SOIL, &Csimple_guiView::OnSoil)
END_MESSAGE_MAP()

// Csimple_guiView construction/destruction

Csimple_guiView::Csimple_guiView()
	: CFormView(Csimple_guiView::IDD)
{
	// TODO: add construction code here

}

Csimple_guiView::~Csimple_guiView()
{
}

void Csimple_guiView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
}

BOOL Csimple_guiView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CFormView::PreCreateWindow(cs);
}

void Csimple_guiView::OnInitialUpdate()
{
	CFormView::OnInitialUpdate();
	GetParentFrame()->RecalcLayout();
	ResizeParentToFit();

	wepsFile = new WepsRunFile("weps.run");

}


// Csimple_guiView diagnostics

#ifdef _DEBUG
void Csimple_guiView::AssertValid() const
{
	CFormView::AssertValid();
}

void Csimple_guiView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

Csimple_guiDoc* Csimple_guiView::GetDocument() const // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(Csimple_guiDoc)));
	return (Csimple_guiDoc*)m_pDocument;
}
#endif //_DEBUG


// Csimple_guiView message handlers

void Csimple_guiView::OnFileOpen()
{
	// TODO: Add your command handler code here
	::MessageBox(NULL,"View open","Test",MB_OK);
}


void Csimple_guiView::onRun()
{
	// TODO: Add your control notification handler code here
	char line[2048];
	char buf2[1024];
	char *onecmd;
	line[0] = '\0';

	strcat(line,"weppweps -W2 -E3");

	system(line);

	// Enable the other buttons
	CWnd *cwnd = GetDlgItem(IDC_WEPS);
	cwnd->EnableWindow();
	cwnd = GetDlgItem(IDC_WEPPMAIN);
	cwnd->EnableWindow();
	cwnd = GetDlgItem(IDC_WEPPEVE);
	cwnd->EnableWindow();
	cwnd = GetDlgItem(IDC_WEPS);
	cwnd->EnableWindow();
	cwnd = GetDlgItem(IDC_PLOT);
	cwnd->EnableWindow();
	cwnd = GetDlgItem(IDC_RUNOFF);
	cwnd->EnableWindow();

	WEPPOutput outwepp("wepp_erosion.out");
	WEPSOutput outweps("gui1_data.out");

	SetDlgItemText(IDC_WEPS_RES,outweps.getTotalAsString());
	SetDlgItemText(IDC_WEPP_RES,outwepp.getTotalAsString());
}

void Csimple_guiView::OnMainWEPS()
{
	char filename[256];
	
	strcpy(filename,"gui1_data.out");

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}

void Csimple_guiView::OnWEPP()
{
	// TODO: Add your control notification handler code here
	char filename[256];
	
	strcpy(filename,"wepp_erosion.out");

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}

void Csimple_guiView::OnEvents()
{
	// TODO: Add your control notification handler code here
	char filename[256];
	
	strcpy(filename,"wepp_eroevents.out");

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}

void Csimple_guiView::OnPlot()
{
	// TODO: Add your control notification handler code here
	char filename[256];
	
	strcpy(filename,"wepp_eroplot.out");

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}

void Csimple_guiView::OnRunoff()
{
	// TODO: Add your control notification handler code here
	char filename[256];
	
	strcpy(filename,"wepp_runoff.out");

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}

void Csimple_guiView::OnInRun()
{
	// TODO: Add your control notification handler code here
	char filename[256];
	
	strcpy(filename,"weps.run");

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}

void Csimple_guiView::OnClimate()
{
	// TODO: Add your control notification handler code here
	char filename[256];
	
	strcpy(filename,wepsFile->getClimateFile());

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}

void Csimple_guiView::OnWind()
{
	// TODO: Add your control notification handler code here
	char filename[256];
	
	strcpy(filename,wepsFile->getWindFile());

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}

void Csimple_guiView::OnMan()
{
	// TODO: Add your control notification handler code here
	char filename[256];
	
	strcpy(filename,"\"");
	strcat(filename,wepsFile->getManageFile());
	strcat(filename,"\"");

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}

void Csimple_guiView::OnSoil()
{
	// TODO: Add your control notification handler code here
	char filename[256];
	
	strcpy(filename,wepsFile->getIFCFile());

	HWND hwnd = GetSafeHwnd();

	ShellExecute(hwnd,"open","wordpad.exe",filename,NULL,SW_SHOWNORMAL);
}
