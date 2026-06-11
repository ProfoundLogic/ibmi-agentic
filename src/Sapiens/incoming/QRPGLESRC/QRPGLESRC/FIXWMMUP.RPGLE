     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE FIXWMMUP - CHECK FOR FORMS ATTACHED IN ERROR
     *========================================================================*
     * FIXWMMUP - Fix program for WMMUP.                                      *
     *                                                                        *
     * PROGRAM DESCRIPTION:                                                   *
     *   Determines if this endorsement form should be attached to the        *
     *   policy.                                                              *
     *                                                                        *
     * CREATED   : 01/13/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : SUPPORT #21390                                             *
     *========================================================================*
     FQSYSPRT   O    F   80        PRINTER oflind(*inOA)
     FWMEML     IF   E           K Disk
     FWDELL     IF   E           K Disk
     FWDELL1    IF   E           K Disk
     F                                     Rename(PEL01:PEL02)
     FWMOOL     IF   E           K Disk
     FWMRSL     IF   E           K Disk
     FWMMUL1    IF   E           K Disk

     D Forms           S             15a   Dim(4) CTDATA
     D xxIncExc        S              1a   Dim(4) CTDATA
     D                 DS
     D xxIncForms                    60a   inz
     D  xxForms                      15a   Dim(4) overlay(xxIncForms)

     D                 DS
     D xxDate                         8s 0 inz
     D  xxYear                        4s 0 overlay(xxDate:5)                    Year
     D  xxMonth                       2s 0 overlay(xxDate:1)                    Month
     D  xxDay                         2s 0 overlay(xxDate:3)                    Day

      ** External data structure
     D psds          ESDS                  ExtName(szq1p)

     D @@Company       S              3
     D @@PolYear       S              3

     D include         S              1
     D x               S              3  0
     D xxNum           S              5  0 Inz(0)
     D xxCompany       S                   Like(EMCO#)
     D xxEnd#          S                   Like(MUEND#)
     D                                     Inz(0)
     D xxPolYear       S                   Like(ELFYR)
     D xxTypeBus       S              3  0

      ** Date check/conversion parms
     D srDateIn        S              8  0
     D srDateOut       S             10
     D srError         S              1
     D srFromFmt       S              8
     D srToFmt         S              8
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @@Company
     C                   Parm                    @@PolYear

      ** Date check/conversion parameters
     C     dtcymd        PList
     C                   Parm                    srDateIn
     C                   Parm      *blanks       srFromFmt
     C                   Parm      '*CYMD'       srToFmt
     C                   Parm                    srDateOut
     C                   Parm                    srError
     *========================================================================*
     * KLIST definitions
     *========================================================================*
     ** Employer Detail file
     C     ELkey         KList
     C                   KFld                    xxCompany
     C                   KFld                    EMFND
     C                   KFld                    EMEMP#
     C                   KFld                    EMDIV
     C                   KFld                    xxPolYear

     C     ELkey2        KList
     C                   KFld                    xxCompany
     C                   KFld                    EMFND
     C                   KFld                    xxPolYear
     C                   KFld                    EMEMP#

     ** Employer Master file
     C     EMkey         KList
     C                   KFld                    xxCompany

     ** Rate Set Code file
     C     RSkey         KList
     C                   KFld                    xxCompany
     C                   KFld                    ELRTCD

     ** Employer Attached Endorsement Forms file
     C     MUkey         KList
     C                   KFld                    xxCompany
     C                   KFld                    ELFND
     C                   KFld                    ELEMP#
     C                   KFld                    ELDIV
     C                   KFld                    ELFYR
     C                   KFld                    xxEnd#
     ** Owner/Officer file
     C     ooKey         klist
     C                   kfld                    xxCompany
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    ELDIV
     *========================================================================*
     * Main
     *========================================================================*
      ** Print the page header.
     C                   Except    Header

      ** Check all employers.
     C     EMkey         Chain     pem01                              60

B1   C                   DoW       *in60 = *off

      ** Only check the state reporting division.
B2   C                   If        EMSTRP = 'Y'

      ** Given policy year.
     C     ELkey         Chain     PEL01                              60
B3   C                   If        *in60 = *off
     C                   Eval      xxIncForms = *blanks

      ** Check for form in question.
     C     MUkey         Chain     PMU01                              60
B4   C                   DoW       *in60 = *off

     C                   Eval      x = 1
B5   C                   Do        4             x
B6   C                   If        MUFORM = Forms(x)

      ** See if the form was attached in error for this policy year.
     C                   EXSR      Valid

      ** If this form should not have been attached, print error report.
B7   C                   If        include = *off
     C                   Eval      xxForms(x) = MUFORM
E7   C                   EndIf                                                  include = *off

E6   C                   EndIf                                                  MUFORM = Forms(x)
E5   C                   EndDo                                                  Do 4 x

     C     MUkey         ReadE     PMU01                                  60
E4   C                   EndDo                                                  MUkey

B5   C                   If        xxIncForms <> *blanks
     C                   Except    Detail1
     C                   If        *inoa = *on
     C                   Except    Header
     C                   Eval      *inoa = *off
     C                   Endif

     C                   Eval      x = 1
B6   C                   Do        4             x

B7   C                   If        xxForms(x) <> *blanks
     C                   Except    Error
     C                   Eval      xxNum = xxNum + 1
     C                   If        *inoa = *on
     C                   Except    Header
     C                   Eval      *inoa = *off
     C                   Endif
E7   C                   EndIf                                                  xxForms(x)<>*blanks

E6   C                   EndDo                                                  Do 4 x

     C                   Eval      xxIncForms = *blanks
E5   C                   EndIf                                                  xxIncForms<>*blanks

E4   C                   EndIf                                                  *in60 = *on

E3   C                   EndIf                                                  EMSTRP = 'Y'

     C     EMkey         ReadE     PEM01                                  60
E2   C                   EndDo                                                  EMkey

     C                   If        xxNum = 0
     C                   Except    None
     C                   EndIf                                                  xxNum = 0

     C                   Eval      *inlr = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     c                   Eval      xxDate = *date
     C                   Move      @@Company     xxCompany
     C                   Move      @@PolYear     xxPolYear

     C                   ENDSR                                                  *inzsr
     *========================================================================*
     *  Valid - Subroutine to determine if this form should be attached.
     *========================================================================*
     C     Valid         BEGSR

     C                   Eval      include = *off

     ** For each division, determine if employer meets criteria.
     C     ELkey2        Chain     PEL02                              60
     C                   Dow       *in60 = *off
     C                   Move      EMTPOB        xxTypeBus
    
     C     RSkey         Chain     PRS01                              60
     C                   If        Forms(x) <> 'WC000308      '
     C                             OR Forms(x) = 'WC000308      '
     C                             AND xxTypeBus >= 2
    
     ** Look for a qualifying Owner/Officer record.
     C     OOkey         Chain     POO01                              60
    
     C                   DoW       *in60 = *off
    
     C                   If            OOEXFL =  xxIncExc(x)
     C                             AND OOSTAT <> 'I'

     C                   If            OOEFDT <= ELPFDT
     C                             AND OOEXDT >  ELPFDT
     C                             OR  OOEFDT <= ELPFDT
     C                             AND OOEXDT =  *zeros

      ** This form should have been attached for this policy year.
     C                   Eval      include = *on
     C                   Leave
     C                   EndIf                                                  OOEFDT = ELPFDT

     C                   EndIf                                                  OOEXFL = xxIncExc(x)

     C     OOkey         ReadE     POO01                                  60
     C                   EndDo                                                  OOkey
    
     C                   EndIf                                                  RSJRST = 'AR'
    
     C                   If        include = *on
     C                   Leave
     C                   EndIf                                                  include = *on
    
     C     ELkey2        ReadE     PEL02                                  60
     C                   EndDo                                                  ELkey
    
     C                   ENDSR                                                  Valid
     *========================================================================*
     OQSYSPRT   E            Header         1  2
     O                       q1pgm               10
     O                                           43 'CHECK ATTACHMENT OF OWNER/'
     O                                           63 'OFFICER ENDORSEMENTS'
     O                       q1user              80

     O          E            Header         2
     O                       xxMonth              2
     O                                            3 '/'
     O                       xxDay                5
     O                                            6 '/'
     O                       xxYear              10
     O                                           74 'PAGE'
     O                       PAGE          4     80

     O          E            Header         2
     O                                           12 'POLICY YEAR:'
     O                       xxPolYear     Z     16

     O          E            Header      1  1
     O                                           05 'GROUP'
     O                                           15 'EMPLOYER#'
     O                                           21 'DIV'
     O                                           35 'ENDORSEMENT'

     O          E            Detail1     0  0
     O                       EMFND         Z      5
     O                       EMEMP#        Z     15
     O                       EMDIV         Z     21

     O          E            Error          1
     O                       xxForms(x)          39

     O          E            None           1
     O                                           12 '*** NONE ***'
     *========================================================================*
** CTDATA Forms
WC000308
WC000310
WC420308
WC420310
** CTDATA xxIncExc
E
I
E
I
