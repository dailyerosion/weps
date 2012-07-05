// simple_guiDoc.cpp : implementation of the Csimple_guiDoc class
//

#include "stdafx.h"
#include "simple_gui.h"

#include "simple_guiDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// Csimple_guiDoc

IMPLEMENT_DYNCREATE(Csimple_guiDoc, CDocument)

BEGIN_MESSAGE_MAP(Csimple_guiDoc, CDocument)
	ON_COMMAND(ID_FILE_OPEN, &Csimple_guiDoc::OnFileOpen)
END_MESSAGE_MAP()


// Csimple_guiDoc construction/destruction

Csimple_guiDoc::Csimple_guiDoc()
{
	// TODO: add one-time construction code here

}

Csimple_guiDoc::~Csimple_guiDoc()
{
}

BOOL Csimple_guiDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}




// Csimple_guiDoc serialization

void Csimple_guiDoc::Serialize(CArchive& ar)
{
	if (ar.IsStoring())
	{
		// TODO: add storing code here
	}
	else
	{
		// TODO: add loading code here
	}
}


// Csimple_guiDoc diagnostics

#ifdef _DEBUG
void Csimple_guiDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void Csimple_guiDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG


// Csimple_guiDoc commands

void Csimple_guiDoc::OnFileOpen()
{
	// TODO: Add your command handler code here
	::MessageBox(NULL,"Help me","open",MB_OK);
}
