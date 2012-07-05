// simple_guiDoc.h : interface of the Csimple_guiDoc class
//


#pragma once


class Csimple_guiDoc : public CDocument
{
protected: // create from serialization only
	Csimple_guiDoc();
	DECLARE_DYNCREATE(Csimple_guiDoc)

// Attributes
public:

// Operations
public:

// Overrides
public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);

// Implementation
public:
	virtual ~Csimple_guiDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	DECLARE_MESSAGE_MAP()
public:
	afx_msg void OnFileOpen();
};


