     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W0066R02 : Group EDI Info Work-with & Maintenance.
     H*****************************************************************
     H*****************************************************************
     H*  DESCRIPTION: Group EDI Info Work-with & Maintenance.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0066R                                                     *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 07/02/10  PROGRAMMER: C:257 PROJECT: WC1790         *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  01/15/14 C:53  Project: 147WCJ
     H*                 Add field for EDI start date override
     H*
     H*****************************************************************
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H*
     Fw0066f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwdf3p     uf a e             disk
     F                                     rename(pf301:dbrcdf)
     Fwdf3l     if   e           k disk
     F                                     rename(pf301:dbrcdf1)
    FSTSTP     if   e             disk
    FSWCPF     if   e             disk
    Fwmfnp     if   e             disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EDI SUBMISSION MAINTENANCE'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Data Structure
    D*---------------------------------------------------------------------
    D#azp             ds
    D#azp1                           5  0
    D#azp2                           4  0
    D*
    D#ph#             DS
    D#ph#1                           3
    D#ph#2                           3
    D#ph#3                           4
    D*
    D#fax             ds
    D#fax1                           3
    D#fax2                           3
    D#fax3                           4
    D*
    D*
    D#zip             ds
    D#zip1                           5  0
    D#zip2                           4  0
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@mode1         s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D @@rrn2          s              9  0
     D kkco#           s                   like(f3co#)
     D kkfnd           s                   like(f3fnd)
     D kkkst           s                   like(f3kst)
     D FLAG            s                   like(f3stat)
     D FLAG1           s                   like(f3stat)
     D FLAG2           s                   like(f3stat)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    flag2
     C                   parm                    @@rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C* Group search
     C     acdpl2        plist
     C     *INKC         parm                    @@KC
     C     *INKL         parm                    @@KL
     C                   parm                    @@MODE1
     C                   parm                    XACO#             3
     C                   parm                    XAFND             3
     C                   parm                    XAFYR             3
    C*================================================================
    C*  KLIST definitions
     C     kwdfl         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkkst
    C*================================================================
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C                   move      '0'           flag
     C                   eval      *in20 = *off
     C                   eval      *in21 = *off
     C                   eval      *in90 = *off
     C                   eval      *in94 = *off
     C*** Beg add *** 01/15/14 ********************************
     C                   eval      *in81 = *off
     C*** End add *** 01/15/14 ********************************
     C* When record is displayed or in edit mode, Seach will not work.
     C     *inkj         ifeq      *on
     C     @@MODE        ANDEQ     'D'
     C     HFLD          IFNE      '     '
     C                   move      '1'           *in94
     C                   move      '1'           *in90
     C                   endif
     C                   endif
     C     *inkj         ifeq      *on
     C     @@MODE        ANDEQ     'C'
     C     HFLD          IFNE      '     '
     C                   move      '1'           *in94
     C                   move      '1'           *in90
     C                   endif
     C                   endif
     C* When record is added search will work.
     C     *inkj         ifeq      *on
     C     @@MODE        ANDEQ     'A'
     C     flag2         ANDne     '1'
     C                   select
     C* When cursor is on Group
     C     HFLD          wheneQ    'SCFND'
     C                   move      ##co#         xaco#
     C                   move      *zero         xafnd
     C                   move      *zero         xafyr
     C                   move      '2'           @@mode1
     C                   call      'W0010R'      acdpl2                 99
     C                   move      xafnd         scfnd
     C                   move      '1'           flag
     C                   move      '1'           *in90
     C                   if        xafnd <>  *zero                               Cursor position
     C                   move      '1'           *in88
     C                   endif
     C* When cursor is on State
     C     HFLD          wheneQ    'SCKST'
     C                   call      'SY601R'
     C                   parm      '2'           ##CALL            1
     C                   parm      '0'           @@KC
     C                   parm      '0'           @@Kl
     C                   parm      *zeros        @@RRN
     C
     C     @@RRN         ifeq      *ZERO
     C                   seton                                        33
     C                   else
     C     @@RRN         chain     PST01                              99
     C                   movel     STKEY         ScKST
     C                   seton                                        33
     C                   endif
     C
     C                   if        stkey <> *blanks
     C                   move      '1'           *in89                          Cursor Position
     C                   endif
     C
     C                   move      '1'           flag
     C                   move      '1'           *in90
     C                   endsl
     C                   endif
     C* When called from Group screen
     C     *inkj         ifeq      *on
     C     @@MODE        ANDEQ     'A'
     C     flag2         ANDeq     '1'
     C                   select
     C* When cursor is on Group
     C     HFLD          wheneQ    'SCFND'
     C                   move      '1'           *in94                          Cursor Position
     C                   move      '1'           *in90
     C* When cursor is on State
     C     HFLD          wheneQ    'SCKST'
     C                   call      'SY601R'
     C                   parm      '2'           ##CALL            1
     C                   parm      '0'           @@KC
     C                   parm      '0'           @@Kl
     C                   parm      *zeros        @@RRN
     C
     C     @@RRN         ifeq      *ZERO
     C                   seton                                        33
     C                   else
     C     @@RRN         chain     PST01                              99
     C                   movel     STKEY         ScKST
     C                   seton                                        33
     C                   endif
     C
     C                   if        stkey <> *blanks
     C                   move      '1'           *in89                          Cursor Position
     C                   endif
     C
     C                   move      '1'           flag
     C                   move      '1'           *in90
     C                   endsl
     C                   endif
     C* Group should not be blank
     C*
     C                   if        scfnd = *zero  and  flag <> '1'
     C                   eval      *in20 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C* The record does not exist for the keys CO#/FND/ST as entered
     C*
     C*
     C                   if        @@mode <> 'C'    and flag <> '1'
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kkfnd = scfnd
     C                   movel     sckst         kkkst
     C     kwdfl         chain     dbrcdf1                            99
     C                   if        *in99 = *off
     C                   eval      *in21 = *on
     C                   eval      *in90 = *on
     C                   endif
     C     scstat        ifne      'A'
     C     scstat        andne     'I'
     C     scstat        andne     ' '
     C                   SETON                                        7990
     C                   endif
     C                   endif
     C*
     C*** Beg add *** 01/15/14 ********************************
     C*
     C     scovdt        ifne      *ZEROS
     C*
     C                   z-add     SCovdt        SRDTI
     C                   call      'SRCVTDAT'    DATCHK
     C                   movel     SRDTO         SCovdt
     C*
B02  C     srerr         ifeq      '1'
     C                   eval      *in81 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   endif
     C*
     C*** End add *** 01/15/14 ********************************
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      *in12 = *off                                 unProtect status
     C                   if        *in10 = *on                                  If mode D or C then
     C                   movel     fnfnd         scfnd                          protect FND
     C                   else
     C                   movel     f3fnd         scfnd
     C                   endif
     C                   move      f3kst         sckst
     C                   move      f3fein        scfein
     C                   move      f3name        scname
     C                   move      f3adr1        scadr1
     C                   move      f3adr2        scadr2
     C                   move      f3cty         sccty
     C                   move      f3st          scst
     C*
     C                   move      f3zip         #zip
     C                   move      #zip1         sczip1
     C                   move      #zip2         sczip2
     C*
     C                   movel     f3azp         #azp
     C                   move      #azp1         scazp1
     C                   move      #azp2         scazp2
     C
     C*
     C                   movel     f3ph#         #ph#
     C                   move      #ph#1         scph#1
     C                   move      #ph#2         scph#2
     C                   move      #ph#3         scph#3
     C*
     C                   movel     f3fax         #fax
     C                   move      #fax1         scfax1
     C                   move      #fax2         scfax2
     C                   move      #fax3         scfax3
     C*
     C                   move      f3info        scinfo
     C                   move      f3ifei        scifei
     C                   move      f3inam        scinam
     C*
     C*
     C                   move      f3icod        scicod
     C                   move      f3tcod        sctcod
     C*** Beg add *** 01/15/14 ********************************
     C*
     C                   Z-ADD     f3ovdt        srdti
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     srdto         scovdt
     C*
     C*** End add *** 01/15/14 ********************************
    C*
    C                   if        @@mode= 'A' AND f3stat = ' '                 Show 'A' default in
     C                   move      'A'           scstat                         F6 (ADD)
     C                   else
     C                   move      f3stat        scstat
     C                   endif
     C                   if        @@mode =  'D' OR @@MODE = 'C'
     C                   move      '1'           *in10
     C                   move      '1'           *in11
     C                   endif
     C                   eval      *in12 = *on                                  Protect status
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   movel     scfnd         f3FND
     C                   move      sckst         f3KST
     C                   move      scfein        f3fein
     C                   move      scname        f3name
     C                   move      scadr1        f3adr1
     C                   move      scadr2        f3adr2
     C                   move      sccty         f3CTY
     C                   move      scst          f3ST
     C                   move      sczip1        #zip1
     C                   move      sczip2        #zip2
     C                   move      #zip          f3zip
     C                   move      scazp1        #azp1
     C                   move      scazp2        #azp2
     C                   movel     #azp          f3azp
     C                   move      scph#1        #ph#1
     C                   move      scph#2        #ph#2
     C                   move      scph#3        #ph#3
     C                   movel     #ph#          f3ph#
     C                   move      scfax1        #fax1
     C                   move      scfax2        #fax2
     C                   move      scfax3        #fax3
     C                   move      #fax          f3fax
     C                   move      scinfo        f3info
     C                   move      scifei        f3ifei
     C                   move      scinam        f3inam
     C                   move      scicod        f3icod
     C                   move      sctcod        f3tcod
     C                   move      scstat        f3stat
     C*** Beg add *** 01/15/14 ********************************
     C*
     C                   Z-ADD     scovdt        srdti
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     srdto         F3ovdt
     C*
     C*** End add *** 01/15/14 ********************************
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   read      swc01                                  95
     C                   if        WCFL09 <> 'Y'
     C                   move      '1'           *in32
     C                   endif
    C                   eval      flag1 = '0'
     C                   eval      @@RRN1 =  @@RRN
     C*                  if        @@mode =  'A' and @@rrn <> 0
     C                   if        @@mode =  'A' and flag2 = '1'
     C     @@rrn2        chain     wmfnp
     C                   move      fnfnd         scfnd
     C                   move      '1'           *in10
     C                   eval      @@rrn = *zeros
     C                   endif
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   if        @@mode = 'S'
     C                   exsr      chgsts
     C                   endif
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   call      comapi        compl
     c                   movel     ##co#         f3co#
     C                   write     dbrcdf
     C                   feod      wdf3p
     C                   endsr
    C*================================================================
    C*CHGSTS  - Change Status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99 = *off
     C                   if        f3stat = 'I'
     C                   move      'A'           f3stat
     C                   else
     C                   move      'I'           f3stat
     C                   endif                                                  tcstat = 'I'
     C                   endif                                                  *in99 = *off
     C*
     C                   update    dbrcdf
     C                   return
     C*
     C                   endsr
