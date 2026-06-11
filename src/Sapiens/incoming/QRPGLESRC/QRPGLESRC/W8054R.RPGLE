     Hcopyright('(c) 2025 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************************
     H* DESCRIPTION: THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW   *
     H*              OR ADD RECORDS IN THE HOLIDAY BLACKOUT DATE FILE *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*            - WORK WITH HOLIDAY BLACKOUT DATES                 *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   W8054R02 - WORK WITH HOLIDAY BLACKOUTDATE UPDATE/DISPLAY    *
     H*                                                               *
     H* CREATED: 05/01/09  PROGRAMMER: C:220 PROJECT: WC1766          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H*                                                               *
     H* 05/16/25 C:344 Project: WC2939                                *
     H*                Added capability to pass-thru/call the new     *
     H*                Holiday Blackout Dates program.                *
     H*                                                               *
     H*****************************************************************
    F*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8054f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtcbp     if   e             disk
     F                                     rename(pcb01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtcbl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pcb01:dbrcdf)
      *** Beg Add ***** 01/13/25 **************************************
     D HolidayBlkOut   pr                  Extpgm('WTHOLBLK')
     D  @@mode                        1
      *** End Add ***** 01/13/25 **************************************
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(20) ctdata perrcd(1)
     D ofd             s             35    dim(20) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Holiday Blackout Dates'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W8054R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
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
     D @@call          s              1
     D kkco#           s                   like(cbco#)
     D kkdate          s                   like(cbdate)
     D @@mode          s              1    inz
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D* DS Fields - START
    D*---------------------------------------------------------------------
    D DSDTA           DS            55
    D  XBDAT                  6     15
    D  XDESC                 23     53
    D  XSTAT                 55     55
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C     dtmdy1        plist
     C                   parm                    srdti
     C                   parm      '*MDY'        srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     *entry        plist
     C                   parm                    @@call
      *** BEG ADD ***** 05/16/25 **************************************
     C                   parm                    @@PassThru        1
      *** END ADD ***** 05/16/25 **************************************
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkdate
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   if        ssstat <> *blank
     C                   seton                                        11
     C                   endif
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*
     C* If position to is not blank, vaidate date
     C*
     C                   IF        SCPST1  <> ' '
     C*
     C                   MOVEL     SCPST1        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        4090
     C*
     C                   ENDIF                                                     Valid date
     C*
     C                   ENDIF                                                     Position to
     C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C     ssstat        ifne      ' '
     C     ssstat        andne     'A'
     C     ssstat        andne     'I'
     C                   seton                                        4390
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   Z-ADD     *LOVAL        kkco#
     C                   MOVE      *LOVAL        kkdate
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C
     C
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL     ##CO#         KKCO#
     C*
     C                   MOVEL     SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         KKDATE
      *
     C                   ELSE
     C                   MOVE      *HIVAL        KKDATE
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL     SFHDDT        KKDATE
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   CLEAR                   SCDATA
     C*
     C                   Z-ADD     CBDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDAT             8
     C*
     C                   EVAL      XBDAT = %SUBST(XXDAT:1:2) + '/' +
     C                             %SUBST(XXDAT:3:2) + '/' + %SUBST(XXDAT:5:4)
     C*
     C                   MOVEL     CBDESC        XDESC
     C                   MOVEL     CBSTAT        XSTAT
     C*
     C                   MOVEL     DSDTA         SCDATA
     C*
     C                   MOVEL     XXDAT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   IF        SRERR = '0'
     C                   MOVEL     SRDTO         SFHDDT            7 0
     C                   ENDIF
     C*
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
     C                   SELECT
     C     SCSEL         WHENEQ    '20'
     C                   EXSR      OPT20
     C                   ENDSL
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*                  SETON                                        LR
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C     @@call        ifeq      '1'
     c                   move      '1'           *in09                          Inquiry
     c                   endif
     C     @@call        ifeq      '2'
     c                   move      '1'           *in30                          Search
     c                   endif
     C**** @@CALL        COMP      '1'                                    09    INQUIRY
      *** Beg Add ***** 05/16/25 **************************************
       If Q1prms < 2;
          @@mode = @@call;
          HolidayBlkOut(@@mode);
          *InLr = *On;
          Return;
       Endif;
      *** End Add ***** 05/16/25 **************************************
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
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
     C*
     C                   IF        SSSTAT <> ' ' and SSSTAT <> CBSTAT
     C*
     C                   EVAL      MEETSS = *OFF
     C*
     C                   ENDIF                                                 Record Display
     C*
     C                   endsr
    C*================================================================
    C* CHANGE STATUS SR IF RECORD SELECTED.
    C*================================================================
     C     OPT20         BEGSR
     C*
     C* OPTION 20=Change status
     C*
     C                   MOVEL     'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                    99
     C*
     C                   ENDSR                                               Opt20
** Column headings
Opt       Blackout Date    Description                  Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1066=                       NNN
1077=                       NNN
1088=                       NNN
11010=                      NNN
11111=                      NNN
11212=                      NNN
11414=                      NNN
11515=                      NNN
11818=                      NNN
12020=Change status         YNN
203F3=Exit                  YYY
205F5=Refresh               YYN
206F6=Add                   YNN
212F12=Cancel               YNN
217F17=Subset               YYN
3
