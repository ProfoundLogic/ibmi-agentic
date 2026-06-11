    H/TITLE   Document Copier
     h DFTACTGRP(*no) ACTGRP(*Caller)
     h option(*nodebugio: *srcstmt)
    H*=========================================================================
    H* (c) 2016 StoneRiver, Inc. All rights Reserved.  -
    H*       StoneRiver is a trademark of StoneRiver, Inc.
    H*=========================================================================
     H* Copies Documents to Save library and back
     H*=========================================================================
     H* Revisions:
     H*
    F*=========================================================================
    F*        F i l e s
    F*=========================================================================
    F*
    F* File Listing - Driver
    F*
     Fdocxref   if   e           k Disk    Rename(docxref:pxr01)
     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const
    D* ------------------------------------------------------------------------
    D*  Variables
    D* ------------------------------------------------------------------------
     D myFile          S             21A   Inz(' ')
     D sqlStatement    S           1000A   Inz(' ')
     D inCall          S              1A
     D strCommand      s            255
    C*
     C     *entry        plist
     C                   parm                    inCall
    C*=========================================================================
    C*      M  A I  N    L  O  G  I  C
    C*=========================================================================
    C*
    C* Read thru File Listing
    C*
      /Free
       // Read thruy file Listing
       Read pxr01;
       DoW Not %Eof(docxref);

          clear strCommand;
          If inCall = 'R';
             If docID <> *Blanks And docID <> '***IMAGE***';
                strCommand = 'CPY OBJ(''/QDLS/winner2/' + %TRIM(docID) +
                ''') TODIR(''/QDLS/' + %Trim(folder) + ''')';
             Else;
                strCommand = 'CPY OBJ(''/QDLS/winner2/' + %TRIM(imgID) +
                ''') TODIR(''/QDLS/' + %Trim(folder) + ''')';
             EndIf;
          Else;
             If docID <> *Blanks And docID <> '***IMAGE***';
                strCommand = 'CPY OBJ(''/QDLS/' + %Trim(folder) + '/' +
                %Trim(docID) + ''') TODIR(''/QDLS/WINNER2'')';
             Else;
                strCommand = 'CPY OBJ(''/QDLS/' + %Trim(folder) + '/' +
                %Trim(imgID) + ''') TODIR(''/QDLS/WINNER2'')';
             EndIf;
          EndIf;
          Monitor;
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-Error;
          EndMon;

          Read pxr01;
       EndDo;

       *InLR = *On;
      /end-free
