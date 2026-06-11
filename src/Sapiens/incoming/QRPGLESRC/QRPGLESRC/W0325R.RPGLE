     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*====================================================================
      /TITLE W0325R - Work with Seasonal Adjustment
     H Bnddir('WTBNDDIR')
      * ***************************************************************
      * DESCRIPTION:  This program allows the user to maintain        *
      *               Seasonal Adjustments on a Claim.                *
      *                                                               *
      * CALLED FROM:  W0070R09                                        *
      * CALLS TO:     NONE                                            *
      * ***************************************************************
    H*  Revisions:
    H*
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
    H*  06/03/10 c:62 Support: 72854
    H*                Correct paging.
    H*
      * ***************************************************************
     Fw0325f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmsap     if   e             disk
     F                                     rename(psa01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmsal     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(psa01:dbrcdf)
     Fsmcol     IF   E           K DISK
     Fwmfnl     IF   E           K DISK
     Fwmeml     IF   E           K DISK
     Fwmupl     IF   E           K DISK
     Fwmcdl     IF   E           K DISK
     FWMCMP     IF   E             DISK
     FWDRBL     IF   E           K DISK
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)

     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9

     D                 DS
     DSCDataDS                 1     70
     D SCDataFDte              1     10
     D SCDataDash             11     11    INZ('-')
     D SCDataTDte             12     21
     D SCDataBlank1           22     33    INZ
     D SCData#Day             34     36
     D SCDataBlank2           37     48    INZ
     D SCDataSavs             49     63

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Seasonal Adjustment'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0325R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D*** Begin Chg *** 06/03/10 *******************************************
     D*sf1pag***       c                   12
     D sf1pag          c                   10
     D*** End   Chg *** 06/03/10 *******************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##saprrn        s              9  0
     D @@call          s              1
     D kkco#           s                   like(saco#)
     D kkfnd           s                   like(safnd)
     D kkfyr           s                   like(safyr)
     D kkcase          s                   like(sacase)
     D kkbgdt          s                   like(sabgdt)
     D kkendt          s                   like(saendt)
     D ssco#           s                   like(saco#)
     D ssfnd           s                   like(safnd)
     D ssfyr           s                   like(safyr)
     D sscase          s                   like(sacase)
     D @@Sav_Chg       S                   Like(SaSaSv)
     D Datefield       s              8  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@Mode            1
     C                   PARM                    ##RRN
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##saprrn
     C                   parm                    ##rrn
     C                   parm                    @@Sav_Chg
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam

    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*** Begin Chg *** 06/03/10 *******************************************
     C*****key1          klist
     C**********         kfld                    kkco#
     C**********         kfld                    kkfnd
     C**********         kfld                    kkfyr
     C**********         kfld                    kkcase
     C*@@@               kfld                    kkbgdt
     C*@@@               kfld                    kkendt
     C     key           klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkbgdt
     C                   kfld                    kkendt
     C*** End   Chg *** 06/03/10 *******************************************

     C     FNKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND

     C     EMKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV

     C     CMKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE

     C     CDKEY         KLIST
     C                   KFLD                    CMSS#

     C     RBKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMJRST
     C                   KFLD                    CMACDT

     C     RBKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMJRST

    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr

     C                   Move      '1'           *in11

     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr

     C*** Begin Del *** 06/03/10 *******************************************
     C**********         Eval      SSco#  = CMco#
     C**********         Eval      SSfnd  = CMfnd
     C**********         Eval      SSfyr  = CMfyr
     C**********         Eval      SScase = CMcase
     C*** End   Del *** 06/03/10 *******************************************

     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr

     C*** Begin Add *** 06/03/10 *******************************************
     C     ##RRN         CHAIN(N)  PCM01                              99
     C*** End   Add *** 06/03/10 *******************************************
     C                   Eval      KKco#  = CMco#
     C                   Eval      KKfnd  = CMfnd
     C                   Eval      KKfyr  = CMfyr
     C                   Eval      KKcase = CMcase
     C*** Begin Add *** 06/03/10 *******************************************
     C                   Eval      KKbgdt = *zeros
     C                   Eval      KKendt = *zeros
     C*** End   Add *** 06/03/10 *******************************************

     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*** Begin Add *** 06/03/10 *******************************************
     C                   Eval      KKbgdt = SCbgdt
     C                   Eval      KKendt = SCendt
     C*** End   Add *** 06/03/10 *******************************************
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C*** Begin Add *** 06/03/10 *******************************************
     C                   Eval      SCbgdt = SAbgdt
     C                   Eval      SCendt = SAendt
     C*** End   Add *** 06/03/10 *******************************************
     C                   Eval      @TOTSV = @TOTSV - @@Sav_Chg
     C                   Eval      @@Sav_Chg = *Zero

     C                   Z-ADD     SABGDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL(P)  SrDtO         DateField
     C                   Eval      SCDataFDte = %editc(DateField:'Y')

     C                   Z-ADD     SAENDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL(P)  SrDtO         DateField
     C                   Eval      SCDataTDte = %editc(DateField:'Y')

     C                   If        SA#DAY <> 0
     C                   Eval      SCData#Day = %editc(SA#DAY:'Z')
     C                   Else
     C                   Eval      SCData#Day = '  0'
     C                   Endif
     C                   Eval      SCDataSavs = %editc(SASASV:'A':'$')
     C                   Move      SCDataDS      SCData
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

      *Get Key Record from WMCMP
     C     ##RRN         CHAIN(N)  PCM01                              99
      * Company Name
     C     CMCO#         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD           80
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      * Fund Name
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      * Employer Name
     C     EMKEY         CHAIN     PEM01                              60
     C                   If        not *in60
      * Concatenate Emp#, Div & Name
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   Endif
      *SSN
     C     CMSS#         IFNE      *BLANK
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scss# = DisplaySS#(MaskData('SSN':'cmss#':cmss#));
      /End-free
      *****  End Chg  ***  06/09/20  **********************
     C                   END
      *  DOI
     C                   Z-ADD     CMACDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL(P)  SrDtO         SCACDT
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCL
     C     SCYRCL        CAT       XACASE:0      SCYRCL

      *  Date of Disability
     C                   Z-ADD     CMDBDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL(P)  SrDtO         SCDBDT
      *  RTW DATE
     C                   Z-ADD     CMRTW         SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL(P)  SrDtO         SCRTW
      *  Claimant Name
     C     CDKEY         CHAIN     PCD01                              60
     C                   If        not *IN60
     C                   MOVE      *BLANKS       SCCNAM
     C     CDLSN         CAT       ',':0         SCCNAM
     C                   CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C                   CAT       CDMID:1       SCCNAM
     C                   CAT       '.':0         SCCNAM
     C                   ENDIF
     C                   ENDIF
      *  AWW, CRT
     C                   MOVE      CMAWW         SCAWW
     C*                  MOVE      CMCRT         SCCRT
     C     RBKY          Setgt     PRB01
     C     RBKY1         Readpe    PRB01                                  60
     C                   If        not *in60
     C                   Eval      SCCCRT = CMAWW * RBCMPR /100
     C                   If        CMUSLH <>'Y'
     C                   If        SCCCRT < RBMNRT
     C                   Eval      SCCCRT = RBMNRT
     C                   Endif
     C                   If        SCCCRT > RBPACP
     C                   Eval      SCCCRT = RBPACP
     C                   Endif
     C
     C                   Else
     C                   If        SCCCRT < RBLHMN
     C                   Eval      SCCCRT = RBLHMN
     C                   Endif
     C                   If        SCCCRT > RBLHMX
     C                   Eval      SCCCRT = RBLHMX
     C                   Endif

     C                   Endif
     C                   Endif

     C                   Z-Add     0             @TOTSV
     C     CMKey         SetLL     WMSAL
     C     CMKey         Reade     WMSAL                                  22
     C                   Dow       Not *in22
     C                   Add       SASASV        @TOTSV
     C     CMKey         Reade     WMSAL                                  22
     C                   Enddo

     C                   Select
     C                   When      @@Mode = 'A' or @@Mode = 'A'
     C                   Eval      @@Call = '1'
     C                   When      @@Mode = 'D'
     C                   Eval      @@Call = '2'
     C                   EndSL

     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C*** Begin Chg *** 06/03/10 *******************************************
     C**********         read      dbrcdf                                 95
     C     key           reade     dbrcdf                                 95
     C*** End   Chg *** 06/03/10 *******************************************
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C*** Begin Chg *** 06/03/10 *******************************************
     C**********         readp     dbrcdf                                 95
     C     key           readpe    dbrcdf                                 95
     C*** End   Chg *** 06/03/10 *******************************************
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr

     C*** Begin Del *** 06/03/10 *******************************************
     C**********         If         SSco#  <> SAco# or
     C**********                    SSfnd  <> SAfnd or
     C**********                    SSfyr  <> SAfyr or
     C**********                    SScase <> SAcase
     C**********         MOVE      *OFF          MEETSS
     C**********         Endif
     C*** End   Del *** 06/03/10 *******************************************

     C                   endsr
** Column headings
Opt  Dates of Seasonal Adjustment  Number of Days   Seasonal Adj Savings
** Description--------------MISN
1011=Select                 NNN
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYY
205F5=Refresh               NNN
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
