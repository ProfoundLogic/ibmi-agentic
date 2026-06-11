     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2008
    H**********************************************************************
    H* PROGRAM: F2306R
    H* DESCRIPTION: Work with DMEPOS Modifier Amounts
    H**********************************************************************
    H*  Created: 06/20/08  C:198  Project: VT007
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H*  08/25/08 C:212 Project:OK019                                      *
    H*                 Take out VT references and make it generic so that *
    H*                 it overrides to the correct state file             *
    H**********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff2306f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     F*** BEG CHG *** 08/25/08 ****************************************
     F*****fmdmpvt   if   e             disk
     Ffmdmp     if   e             disk    USROPN
     F*** END CHG *** 08/25/08 ****************************************
     F                                     rename(pdm01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     F*** BEG CHG *** 08/25/08 ****************************************
     F*****fmdmlvt   if   e           k disk
     Ffmdml     if   e           k disk    USROPN
     F*** END CHG *** 08/25/08 ****************************************
     F                                     infds(dbfds)
     F                                     rename(pdm01:dbrcdf)
     F*** BEG CHG *** 08/25/08 ****************************************
     F*****fmprlvt   if   e           k disk
     Ffmprl     if   e           k disk    USROPN
     F*** END CHG *** 08/25/08 ****************************************
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
     D*** BEG ADD *** 08/25/08 ****************************************
     D QCA             s             56    dim(6) ctdata perrcd(1)
     D*
     D                 DS                  INZ
     DQQCMD                    1     56A
     DQQST                    38     39A
     D*
     D*** END ADD *** 08/25/08 ****************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'DMEPOS Modifier Amounts'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2306R02'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
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
     D kkco#           s                   like(dmco#)
     D kkprcd          s                   like(dmprcd)
     D kkefdt          s                   like(dmefdt)
     D kkmod1          s                   like(dmmod1)
     D kkmod2          s                   like(dmmod2)
     D xxmra           s              9
     D*** BEG ADD *** 08/25/08 ****************************************
     D length          s             15  5 INZ
     D*** END ADD *** 08/25/08 ****************************************
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
     C                   parm                    @@call
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    kkco#
     C                   parm                    kkprcd
     C                   parm                    kkefdt
     C*** BEG ADD *** 08/25/08 ****************************************
     C                   parm                    @@ST              2
     C*** END ADD *** 08/25/08 ****************************************
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
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
    C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkprcd
     C                   kfld                    kkefdt
     C********           kfld                    kkmod1
     C********           kfld                    kkmod2
     C*
     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkprcd
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
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
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
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
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scprcd = dmprcd
     C                   z-add     dmefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         scpsdt
     C                   endif
     C     prky          chain     ppr01
     C*** BEG CHG *** 08/25/08 ****************************************
     C*****              if        %found(fmprlvt)
     C                   if        %found(fmprl)
     C*** END CHG *** 08/25/08 ****************************************
     C                   movel     prdsc1        scpstd
     C                   endif
     C                   eval      scmod1 = dmmod1
     C                   eval      scmod2 = dmmod2
     C                   eval      scmra  = dmmra
     C                   eval      scstat = dmstat
     C                   evalr     xxmra = %char(scmra)
     C*
     C                   eval      scdata = ' '+%trim(scmod1)+'              '
     C                             + scmod2 + '          '
     C                             + xxmra + '       '
     C                             +  %trim(scstat)
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
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*** BEG ADD *** 08/25/08 ****************************************
     C*** DELETE OVERRIDE TO FMDMPXX, FMDMLXX, FMPRLXX FILES
     C                   CLOSE     FMDMP
     C                   CLOSE     FMDML
     C                   CLOSE     FMPRL
     C                   EXSR      DLOVRD
     C*
     C                   eval      *inlr = *on
     C*** END ADD *** 08/25/08 ****************************************
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*** BEG DEL *** 08/25/08 ****************************************
     C********           eval      *inlr = *on
     C*** END DEL *** 08/25/08 ****************************************
     C*** BEG ADD *** 08/25/08 ****************************************
     C*** OVERRIDE TO FMDMPXX, FMDMLXX, FMPRLXX FILES
     C                   EXSR      OVRIDE
     C                   OPEN      FMDMP
     C                   OPEN      FMDML
     C                   OPEN      FMPRL
     C*** END ADD *** 08/25/08 ****************************************
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key1          reade     dbrcdf                                 95
     C*
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key1          readpe    dbrcdf                                 95
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
     C                   endsr
     C*** BEG ADD *** 08/25/08 ****************************************
     C*================================================================
     C* OVRIDE - Overide files based on adjudication state
     C*================================================================
     C     OVRIDE        BEGSR
     C*
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL(P)  @@ST          QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM      56            LENGTH
     C*
     C                   MOVEL(P)  QCA(2)        QQCMD
     C                   MOVEL(P)  @@ST          QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM      56            LENGTH
     C*
     C                   MOVEL(P)  QCA(3)        QQCMD
     C                   MOVEL(P)  @@ST          QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM      56            LENGTH
     C*
     C                   ENDSR
     C*================================================================
     C* DLOVRD - Delete file overrides
     C*================================================================
     C     DLOVRD        BEGSR
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*** END ADD *** 08/25/08 ****************************************
** Column headings
  Modifier 1      Modifier 2            MRA     Status
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
** CTDATA QCA
OVRDBF FILE(FMDMP) TOFILE(*LIBL/FMDMP  )
OVRDBF FILE(FMDML) TOFILE(*LIBL/FMDML  )
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FMDMP)
DLTOVR FILE(FMDML)
DLTOVR FILE(FMPRL)
