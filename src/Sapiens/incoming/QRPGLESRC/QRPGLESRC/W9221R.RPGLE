     h*  (C) Copyright FISERV Insurance Solutions, Inc., 2007                                      *
     h*============================================================================================*
      /title W9221R - Batch Auto Cash Dissolve Process
     h option(*nodebugio: *srcstmt) debug(*yes)
     h*============================================================================================*
     h*  DESCRIPTION: Create and A/R batch in file WDEAP using the auto cash-dissolve hierarchy    *
     h*               defined in WW Groups-Option 61, the auto cash-dissolve work file WXCZP and   *
     h*               API WA055C.                                                                  *
     h*                                                                                            *
     h*               To Accomplish this, the following is done:                                   *
     h*               a. Create a check header Record in WDEAP and populate it.                    *
     h*               b. Call WA055C with following parameters:                                    *
     h*                    1. Calltype, value = "C"                                                *
     h*                    2. RRNC, value = the relative recore number of the check header record  *
     h*                       written in step "a" aboce.                                           *
     h*               c. The WA055C API will create temporary check detail records in file WZEAP.  *
     h*                  These records will contain the dissolved payments.                        *
     h*               d. Loop through the WZEAP file, and for each record read in WZEAP, clear and *
     h*                  populate the check detail record format for file WDEAP after open WZEAP,  *
     h*                       then close WZEAP file after finish this employer's dissovle records. *
     h*               e. Delete the check header record after all detail records for the check     *
     h*                  being processed have been written to WDEAP.                               *
     h*               f. When all WXCZP records have been processed, update total batch amount for *
     h*                  all WDEAP records.                                                        *
     h*               e. Close QSYSPRT, if opened.                                                 *
     h*                                                                                            *
     h*  CALLED FROM: MENU                                                                         *
     h*                                                                                            *
     h*  CALLS TO:    WA055C                                                                       *
     h*                                                                                            *
     h*  CREATED: 02/12/07  PROGRAMMER: C:132 PROJECT: 91WOB                                       *
     h*============================================================================================*
     h*  REVISIONS:                                                                                *
     h*                                                                                            *
     h*  04/12/07 C:136 Support: C091 57972                                                       *
     h*                 Get future year if ELFYR doesn't exist as of current date.                 *
     h*                                                                                            *
     h*  04/10/07 C:136 Support: WA091 57930                                                       *
     h*                 Execute a read instead of a chain to WXCZP in case first record gets       *
     h*                 deleted.                                                                   *
     h*                                                                                            *
     h*============================================================================================*
     f*
     f*  Physical file definition (for incoming flat file)
     fwxczp     if   e             disk
     f*
     f*  Primary logical file definition (for retrieving auto cash-dissolve)
     fwzeal     if   e           k disk    prefix(Z) usropn
     f                                     rename(pea01:pea03)
     f*
      *  Logical file definition (for maintaining record)
     fWDEAL4    UF   E           K DISK
     f*
     f*  Physical file for adding batch cash dissolve records & Chk Hdr Record
     fwdeap     uf a e             disk    rename(pea01:dbrcdf)
     f                                     infds(dbfds)
     f*
     FWDELL2    IF   E           K DISK    RENAME(PEL01:PEL0L2)
     f*
     FWTABL     IF   E           K DISK    IGNORE(PAT01)
     f                                     ignore(pba01)
     f                                     ignore(pbc01)
     f                                     ignore(pbs01)
     f                                     ignore(pb201)
     f                                     ignore(pb701)
     f                                     ignore(pca01)
     f                                     ignore(pcc01)
     f                                     ignore(pcn01)
     f                                     ignore(pcs01)
     f                                     ignore(phy01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     f                                     ignore(plr01)
     f                                     ignore(pna01)
     f                                     ignore(pnc01)
     f                                     ignore(ppa01)
     f                                     ignore(ppc01)
     f                                     ignore(prc01)
     f                                     ignore(prs01)
     f                                     ignore(prt01)
     f                                     ignore(psp01)
     f****************                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)
     f*
     fqsysprt   o    f  132        printer oflind(*inoa)
     f                                     usropn
     f*
     d*================================================================
     d*  stand alone fields - top
     d*================================================================
      *
     D OpnPrt          s              1    INZ(' ')
     D WZOPEN          s              1    INZ(' ')
     D ErrMsg          s             40
     D ErrCnt          s              5  0 inz(0)
     D CHKRRN          s              9  0 inz(0)
     D XXRRN           s              9  0 inz(0)
     D xxBchTot        s             11  2 inz(0.00)
     D xxChkTot        s             11  2 inz(0.00)
     d sysTime         s                   like(EATIME)
     d SvTRDT          s                   like(EATRDT)
     d SvBAT#          s                   like(EABAT#)
     d NNBAT#          s                   like(EABAT#) INZ(*ZEROS)
     d xxFyr           s                   like(EAFYR)
     d xxSYCD          s                   like(EASYCD)
      *
     D usadte          s              8  0
     D sysdte          s              8  0
     D mdyydte         s              8  0
     D UDTE            s              8  0
     D mdydte          s              6  0
     D ymddte          s              6  0
     D cymddte         s              7  0
     D sysdt7          s              7  0
     D cltID           s              3
     D xsysdt          s                   like(eadt)
     D sysDate         s               d   datfmt(*iso)
     D usaDate         s               d   datfmt(*usa)
     D mdyyDate        s               d   datfmt(*usa)
     D mdyDate         s               d   datfmt(*mdy)
     D ymdDate         s               d   datfmt(*ymd)
    D*---------------------------------------------------------------------
     D edit_mask       s             10
     D usa_date_n      s             10
     D usa_date        s               d   datfmt(*usa)
     D Datusa          S               D   Datfmt(*Usa)
     d*
      *================================================================
      * Data Structures
      *================================================================
      *
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
     D psds          esds                  extname(szq1p)
      *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
      *================================================================
      *  /COPY required constants
      *================================================================
     D COMAPI          C                   CONST('WA001R')
      *
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    CZCO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*
      *================================================================
      *  KLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@frdt            7
     C*
     C     EAKY          KLIST
     C                   KFLD                    CZCO#
     C                   KFLD                    EABAT#
     C*
     C     EAKY1         KLIST
     C                   KFLD                    CZCO#
     C                   KFLD                    NNBAT#
     C*
     C     cbky          KLIST
     C                   KFLD                    CZCO#
     C*
     C     ELKY2         KLIST
     C                   KFLD                    CZCO#
     C                   KFLD                    CZFND
     C                   KFLD                    CZEMP#
     C                   KFLD                    CZDIV
     C                   KFLD                    sysDt7
      *
     C     ELKY          KLIST
     C                   KFLD                    CZCO#
     C                   KFLD                    CZFND
     C                   KFLD                    CZEMP#
     C                   KFLD                    CZDIV
      *
      *  WZEAP Key
      *
     C     ZEKY          KLIST
     C                   KFLD                    CZCO#
     C                   KFLD                    CZFND
     C                   KFLD                    CZEMP#
     C                   KFLD                    CZDIV
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
      *
     C                   call      'RTVCLTID'
     C                   parm                    cltID
      *
     C                   TIME                    DSDTTM
     C                   EVAL      SYSTIME = DSTIME
      * Format = *iso
      *
     C     *USA          move      DSDATE        sysDate                        yyyy-mm-dd
     C     *cymd         move      sysDate       cymddte                        cyymmdd
     C     *cymd         move      sysDate       sysdt7                         cyymmdd
     C                   move      sysdt7        @@frdt                         cyymmdd
      *
     C     *cymd         move      sysdt7        usa_date
     C                   move      usa_date      usa_date_n                     MMDDYYYY
      *
      *================================================================
      * MAINLINE - Main section of  program processing
      *================================================================
      *
      *  Chain to Flat file for first record and process until done
      *
      *** beg chg ***** 04/10/07 ***************************************
     C*****1*****        chain     wxczp                              60
     C                   read      wxczp                                60
      *** end chg ***** 04/10/07 ***************************************
      *
      * If no record found, Print an error message
      *
     C                   IF        *IN60
     C                   Clear                   ErrMsg
     C                   Eval      ErrMsg = 'NO Records in Batch file WXCZP'
     C                   Exsr      PrtErrMsg
     C                   Exsr      srEndPgm
     C                   EndIF
      *
      *  Determine last used batch number, if records in WXCZP file.
      *
     C                   SUB       NNBAT#        NNBAT#
     C                   MOVE      *HIVAL        EABAT#
     C     EAKY          SETGT     WDEAL4
     C     CZCO#         READPE(N) WDEAL4                                 99
      *
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EABAT#        NNBAT#
     C                   ENDIF
     C                   Eval      NNBAT# = NNBAT# + 1
      *
     C                   CLEAR                   DBRCDF
      *
     C                   DOW       not *in60
      *
      *  setup Check header Record in WDEAP
     C                   exsr      CrtChkHdr
      *
      *  Now call the API to generate the WZEAP Records
     C                   exsr      APICrtRcd
      *
      *  Now use WZEAP records to create the detail WDEAP Records
     C                   exsr      CrtDtlRcd
      *
      *  Now Delete the Check header Record in WDEAP
     C                   exsr      DltChkHdr
      *
      *  Read for additional records in the Batch auto-dissolve file.
     C                   read      wxczp                                  60
     C                   EndDo
      *
     C                   exsr      srEndPgm
      *
      *================================================================
      * srENDPGM - End of program processing
      *================================================================
      *
     C     srEndPgm      begsr
      *
      *  Update records with batch total.
      *
     C                   IF        xxBchTot <> *zeros
     C                   exsr      UpdBchTot
     C                   endif
      *
      *  Batch total Amount should match Check Amount
      *
     C                   IF        xxBchTot <> xxChkTot AND xxBchTot <> *zeros
     C                   clear                   ErrMsg
     C                   eval      ErrMsg = 'Batch Amt UNEQUAL Check Amt'
     C                   exsr      PrtErrMsg
     C                   endif
      *
      *  Close Qsysprt if opened during processing
      *
     C                   If        OpnPrt = 'Y'
     C                   Time                    DsDtTm
     C                   Except    LrTotal
     C                   Close     Qsysprt
     C                   EndIF
      *
      *  Set on LR indicator and exit program
      *
     C                   move      *on           *inlr
     C                   Return
      *
     C                   endsr
      *================================================================
      * APICrtRcd - Create WZEAP Records using the API for auto cash
      *================================================================
      *
     C     APICrtRcd     begsr
     C*
     c                   eval      ##call = 'C'
     c                   eval      earrn = dbrrn
     c                   call      'WA055C'
     c                   parm                    ##call            1
     c                   parm                    earrn             9 0
     C*
     C                   endsr
      *
      *================================================================
      * CrtChkHdr - Create Check Header Record in WDEAP
      *================================================================
      *
     C     CrtChkHdr     begsr
     C*
     C*  Get the latest policy year for Check Header record
     C*
     C     elky2         setgt     pel0l2
     C     eLky          readpe    pel0l2
      *** beg chg ***** 04/12/07 ********************************
     C***********        eval      xxfyr = elfyr
     C                   if        not(%eof)
     C                   eval      xxfyr = elfyr
     C                   else
     C     elky2         setll     pel0l2
     C     elky          reade     pel0l2
     C                   if        not(%eof)
     C                   eval      xxfyr = elfyr
     C                   else
     C                   eval      xxfyr = 0
     C                   endif
     C                   endif
      *** end chg ***** 04/12/07 ********************************
     C*
     C*  Get the system type code for Check Header record
     C*
     C     CZCO#         SETLL     PSY01
     C     CZCO#         READE     PSY01                                  60
     C     *IN60         DOUEQ     '1'
     C     SYTYPE        IFEQ      'Y'
     C     SYSTAT        ANDEQ     'A'
     C                   MOVEL     SYKEY         EASYCD
     C                   MOVEL     SYKEY         XXSYCD
     C                   LEAVE
     C                   ENDIF
     C     CZCO#         READE     PSY01                                  60
     C                   ENDDO
      *
      *  Load the record fields for check header record
      *
     C                   eval      eaco#  = czco#
     C                   eval      eabat# = nnbat#
     C                   eval      eabata = *zeros
     C                   eval      eafnd  = czfnd
     C                   eval      eafyr  = xxfyr
     C                   eval      eaemp# = czemp#
     C                   eval      eadiv  = czdiv
     C                   eval      eaarcd = *blanks
     C                   eval      eadesc = *blanks
     C                   eval      eapodt = *zeros
     C                   eval      eachk# = czchk#
     C                   eval      eackam = czckam
     C                   eval      eaamt  = *zeros
     C                   eval      eatfap = 'N'
     C                   eval      eatffl = 'N'
     C                   eval      eatfdt = *zeros
     C                   eval      eatfam = *zeros
     C                   eval      eastat = *blanks
     C                   eval      eainv# = *zeros
     C                   eval      eatrdt = *zeros
     C                   eval      ealate = czlate
     C                   eval      eabb#  = *blanks
     C                   eval      eaachf = *blank
     C                   eval      eaachd = *zeros
     C                   eval      eaacht = *zeros
     C                   eval      eaachu = *blanks
     C                   eval      eabbdt = *zeros
     C                   eval      eactyp = *blank
     C                   eval      eacash = 'Y'
     C                   eval      eardiv = *zeros
     C                   eval      eaautr = *blank
      *
      *  Add each check header amount to Check Total
     C                   add       eackam        xxChkTot
      *
      *  Load the record maintenance fields
      *
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      Q1USER        EAUSER
     C                   Z-ADD     SYSDT7        EADT
     c                   eval      sysTime = sysTime + 1
     C                   MOVEL     SysTime       EATIME
      *
     C                   Z-ADD     SYSDT7        EAIDT
     C                   MOVE      Q1PGM         EAIPGM
     C                   MOVE      Q1USER        EAIUSR
      *
     C                   MOVE      NNBAT#        EABAT#
      *
     C                   WRITE     DBRCDF
      *
      *  Save the Check Relative Record Number.
     c                   eval      CHKRRN = dbrrn
      *
     C                   endsr
      *================================================================
      * CrtDtlRcd - Create Detail Check Records in WDEAP
      *================================================================
      *
     C     CrtDtlRcd     begsr
     C*
     C                   open      wzeal
     C                   eval      WZOPEN = 'Y'
     C*
     C     zeky          chain     wzeal                              60
     C*
     C* If no WZEAP records were created, print error msg and end program
     C*
     C                   If        *IN60
     C                   Clear                   ErrMsg
     C                   Eval      ErrMsg = 'NO Records Found for WZEAP'
     C                   exsr      PrtErrMsg
     C                   exsr      srEndPgm
     C                   EndIf
     C*
     C                   DOW       NOT *IN60
     C*
     C                   eval      eaco#  = czco#
     C                   eval      eabat# = nnbat#
     C                   eval      eabata = *zeros
     C                   eval      eafnd  = czfnd
     C                   eval      eafyr  = zeafyr
     C                   eval      eaemp# = czemp#
     C                   eval      eadiv  = czdiv
     C                   eval      eaarcd = zeaarcd
     C                   eval      eadesc = zeadesc
     C                   eval      eapodt = zeapodt
     C                   eval      eachk# = czchk#
     C                   eval      eackam = czckam
     C                   eval      eaamt  = zeaamt
     C                   eval      easycd = xxsycd
     C                   eval      eatfap = 'N'
     C                   eval      eatffl = 'N'
     C                   eval      eatfdt = *zeros
     C                   eval      eatfam = *zeros
     C                   eval      eastat = *blanks
     C                   eval      eainv# = *zeros
     C                   eval      eatrdt = *zeros
     C                   eval      ealate = czlate
     C                   eval      eabb#  = *blanks
     C                   eval      eaachf = *blank
     C                   eval      eaachd = *zeros
     C                   eval      eaacht = *zeros
     C                   eval      eaachu = *blanks
     C                   eval      eabbdt = *zeros
     C                   eval      eactyp = zeactyp
     C                   eval      eacash = 'D'
     C                   eval      eardiv = *zeros
     C                   eval      eaautr = *blank
      *
      *  Load the record maintenance fields
      *
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      Q1USER        EAUSER
     C                   Z-ADD     SYSDT7        EADT
     c                   eval      sysTime = sysTime + 1
     C                   MOVEL     SysTime       EATIME
      *
     C                   Z-ADD     SYSDT7        EAIDT
     C                   MOVE      Q1PGM         EAIPGM
     C                   MOVE      Q1USER        EAIUSR
     C*
     C                   eval      xxBchTot = XXBchTot + ZEAAMT
      *
     C                   WRITE     DBRCDF
      *
     C     ZEKY          READE     WZEAL                                  60
     C                   EndDo
      *
      *  Close WZEAL if opened during processing
      *
     C                   If        WZOPEN = 'Y'
     C                   Close     WZEAL
     C                   EndIF
     C*
     C                   endsr
      *
      *================================================================
      * DltChkHdr - Delete Check Header Record in WDEAP
      *================================================================
      *
     C     DltChkHdr     begsr

     C     ChkRrn        chain     DBRCDF                             60
     C*
     C                   IF        *IN60
     C                   clear                   ErrMsg
     C                   eval      ErrMsg = 'Check Header Record NOT Found'
     C                   exsr      PrtErrMsg
     C                   ELSE
     C                   Delete    DBRCDF
     C                   EndIf
     C*
     C                   endsr
      *
      *================================================================
      * UpdBchTot - Update WDEAP Detail recors with Batch Total Amt
      *================================================================
      *
     C     UpdBchTot     begsr
     C*
     C     EAKY1         chain     pea01                              60
     C*
     C                   If        *IN60
     C                   Clear                   ErrMsg
     C                   Eval      ErrMsg = 'Update of Batch Amount Failed'
     C                   exsr      PrtErrMsg
     C                   EndIf
     C*
     C                   DOW       NOT *IN60
     C*
     C                   Eval      EABATA  = xxBchTot
     C                   Update    pea01
     C*
     C     EAKY1         ReadE     pea01                                  60
     C                   EndDO
     C*
     C                   endsr
      *
      *================================================================
      * PrtErrMsg - Open Qsysprt and print if have an error
      *================================================================
      *
     C     PrtErrMsg     begsr
     C*
     C                   If        OpnPrt <> 'Y'
     C                   Open      Qsysprt
     C                   Except    Header
     C                   eval      OpnPrt = 'Y'
     C                   EndIF
     C*
     C                   eval      ErrCnt = ErrCnt + 1
     C                   Except    Detail
     C*
     C                   endsr
      *
      *================================================================
     OQsysprt   e            Header         1 02
     O                       dstime               9 '  :  :  '
     O                                           60 'Batch Auto Cash-Dissolve '
     o                                              'Error Report'
     O                       dsdate             130 '  /  /    '
     O          e            Header         2
     O                                            7 'W9221R'
     O                                          126 'PAGE'
     O                       page          z    130
     O          e            Header         2
     O                                           34 'Error Description'
     O                                           52 'GRP'
     O                                           62 'EMP #'
     O                                           68 'DIV'
     O                                           72 'Pyr'
     O                                           80 'CHK #'
     O                                           90 'CHK AMT'
     O                                          102 'BATCH #'
     O                                          118 'BATCH AMT'
     O
     O          e            Detail         2
     O                                           16 'Error Message:'
     O                       ErrMsg           +   1
     O                       CzFnd               52
     O                       CzEmp#        3     62
     O                       CzDiv         3     68
     O                       xxFyr               72
     O                       CzChk#        3     80
     O                       CzCkAm        c     92
     O                       nnbat#        3    102
     O                       xxBchTot      c    118
     O
     O          e            LrTotal     1  1
     O                                           16 'Total Errors:'
     O                       ErrCnt        z  +   1
     O                                           40 'Completed at:'
     O                       DsDtTm           +   1 '  :  :  &  /  /    '
