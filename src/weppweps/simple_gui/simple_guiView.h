// simple_guiView.h : interface of the Csimple_guiView class
//


#pragma once


extern class WepsRunFile;
class Csimple_guiView : public CFormView
{
protected: // create from serialization only
	Csimple_guiView();
	DECLARE_DYNCREATE(Csimple_guiView)

public:
	enum{ IDD = IDD_SIMPLE_GUI_FORM };

// Attributes
public:
	Csimple_guiDoc* GetDocument() const;

// Operations
public:

// Overrides
public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnInitialUpdate(); // called first time after construct

// Implementation
public:
	virtual ~Csimple_guiView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	WepsRunFile *wepsFile;

// Generated message map functions
protected:
	DECLARE_MESSAGE_MAP()
public:
	afx_msg void OnFileOpen();
//	afx_msg void OnBnClickedButton1();
	afx_msg void onRun();
	afx_msg void OnMainWEPS();
	afx_msg void OnWEPP();
	afx_msg void OnEvents();
	afx_msg void OnPlot();
	afx_msg void OnRunoff();
	afx_msg void OnInRun();
	afx_msg void OnClimate();
	afx_msg void OnWind();
	afx_msg void OnMan();
	afx_msg void OnSoil();
};

#ifndef _DEBUG  // debug version in simple_guiView.cpp
inline Csimple_guiDoc* Csimple_guiView::GetDocument() const
   { return reinterpret_cast<Csimple_guiDoc*>(m_pDocument); }
#endif

