     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('QC2LE')
      //====================================================================
     H/TITLE ONBASERCD - Write the index record for the OnBase processing
      //====================================================================
      //  Description:
      //
      //  This program is called by STLETTER via the RunDataPgm subroutine.
      //  It is used to write the index record for the On Base processing.
      //
      //  Created:  04/22/16  C:302  Project: 25WUK
      //====================================================================
      //  Revisions:
      //
      //  09/06/17 C:65  Project: 25WUK/Phw#49246
      //                 The flow file names have changed so access the
      //                 output file name to compensate.
      //
      //  08/30/16 C:65  Project: 25WUK
      //                 Change the index file to be named the same as the
      //                 PDF but with a .txt extension.
      //
      //====================================================================

     fXMLprel1  IF   E           K Disk
     fstscp     IF   E             Disk
     fwmcml1    IF   E           K Disk
     fstctl1    IF   E           K Disk

     d psds          esds                  ExtName(szq1p)

     d Record          s            256
     d PipeDelimiter   c                   Const('|')
     d DateInjury      s              8A
     d CRLF            c                   Const(x'0d25')
     d*** begin add ***  08/30/16  *************************************
     d indexFile       s             60A
     d*** end add *****  08/30/16  *************************************

     d                 ds
     d  KEYc                   1     40
     d  Claim                  4     31
     d  CO#c                   1      3  0
     d  Fndc                   4      6  0
     d  Fyrc                   7      9  0
     d  Casec                 10     16  0

     d                 ds
     d FileName                      60
     d  CreationDate                  8    Overlay(FileName:19)
     d  CreationTime                  6    Overlay(FileName:27)

     d OnBaseRcd       pr
     d                               60

     d OnBaseRcd       pi
     d   XMLFileName                 60

     d WrtiteIfs       PR
     d  DataRecord                  256A

      *--------------------------------------------------------------------
      * Close a file
      * int close(int fildes)
      *--------------------------------------------------------------------
     D close           pr            10I 0 ExtProc('close')
     D  fildes                       10I 0 value

      *--------------------------------------------------------------------
      * Open a File
      * int open(const char *path, int oflag, . . .);
      *--------------------------------------------------------------------
     D open            pr            10I 0 ExtProc('open')
     D  path                           *   value options(*string)
     D  openflags                    10I 0 value
     D  mode                         10U 0 value options(*nopass)
     D  ccsid                        10U 0 value options(*nopass)
     D  txtcreatid                   10U 0 value options(*nopass)

      *--------------------------------------------------------------------
      * Write to a file
      * ssize_t write(int fildes, const void *buf, size_t bytes)
      *--------------------------------------------------------------------
     D write           pr            10I 0 ExtProc('write')
     D  fildes                       10i 0 value
     D  buf                            *   value
     D  bytes                        10U 0 value

      /Free

       Read STSCP;
       //*** begin chg ***  08/30/16  *************************************
       //*****If SCurl9 = *blanks or SCdsc9 = *Blanks;
       If SCurl9 = *blanks;
       //*** end chg *****  08/30/16  *************************************
          *inlr = *on;
          Return;
       Endif;

       Clear Record;

       //*** begin del ***  09/06/17  *************************************
       //*****FileName  = %Trim(%Replace('.PDF':XMLFileName
       //*****       :%Scan('.XML' : %TRIMR(XMLFileName )):4));
       //*** end del *****  09/06/17  *************************************

       Chain(n) (XMLFileName) XMLprel1;

       //*** begin add ***  08/30/16  *************************************
       indexFile = %Trim(%Replace('.TXT':XPFile
              :%Scan('.PDF' : %TRIMR(XPFile)):4));
       //*** end add *****  08/30/16  *************************************
       //*** begin add ***  09/06/17  *************************************
       FileName  = XPFile;
       //*** end add *****  09/06/17  *************************************

       Chain (XPco#:XPfnd:XPfyr:XPcase) wmcml1;

       Chain (XPrmcd) stctl1;

       CO#c =  XPCO#;
       Fndc  = XPFnd;
       Fyrc  = XPFyr;
       Casec = XPCase;

       DateInjury = %char(%date(CMacdt:*cymd):*iso0);

       Record = %Trim(Record) + %Trim(Claim) + PipeDelimiter;
       Record = %Trim(Record) + %Trim(CreationDate) + PipeDelimiter;
       Record = %Trim(Record) + %Trim(CreationTime)  + PipeDelimiter;
       Record = %Trim(Record) + %Trim(DateInjury) + PipeDelimiter;
       Record = %Trim(Record) + %Trim(XPrmcd) + PipeDelimiter;
       Record = %Trim(Record) + %Trim(CTdesc)  + PipeDelimiter;
       Record = %Trim(Record) + %Trim(FileName) + CRLF;

       WrtiteIfs(Record);

       *inlr = *on;
       Return;

      /End-Free

      * --------------------------------------------------
      * Procedure name: WrtiteIfs
      * --------------------------------------------------
     P WrtiteIfs       b
     D WrtiteIfs       pi
     D  DataRecord                  256A

     D fd              s             10I 0
     d*** begin chg ***  08/30/16  *************************************
     D***** IFSFileName     s             64A
     D IFSFileName     s            250A
     d*** end chg *****  08/30/16  *************************************
     D Flags           s             10U 0
     D Mode            s             10U 0

     D O_CREAT         C                   8
     D O_WRONLY        C                   2
     D O_TRUNC         C                   64
     D O_APPEND        C                   256
     D O_CODEPAGE      C                   8388608
     D O_TEXTDATA      C                   16777216
     D S_IRWXU         C                   455
     D S_IRWXG         C                   56

      /Free

       //*** begin chg ***  08/30/16  *************************************
       //*****IFSFileName = %Trim(SCurl9) + %Trim(SCdsc9);
       IFSFileName = %Trim(SCurl9) + %Trim(indexFile);
       //*** end chg *****  08/30/16  *************************************
       Flags = O_WRONLY + O_CREAT + O_APPEND + O_CODEPAGE;
       Mode =  S_IRWXU + S_IRWXG;
       fd = open(%Trimr(IFSFileName):Flags :Mode:819);
       callp close(fd);

       Flags = O_WRONLY + O_APPEND + O_TEXTDATA;
       fd = open(%Trimr(IFSFileName):Flags);

       Callp write(fd: %Addr(DataRecord): %Len(%Trimr(DataRecord)));
       Callp close(fd);

       Return;

      /End-Free

     P WrtiteIfs       E

