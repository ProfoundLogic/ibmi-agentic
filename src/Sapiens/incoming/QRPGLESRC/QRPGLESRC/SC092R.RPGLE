     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SC092R - WORK WITH KEY TYPES/INDEXES
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *                                                                        *
     *                                                                        *
     *                                                                        *
     *                                                                        *
     * CALLED BY : MENU                                                       *
     *                                                                        *
     * CALLS  TO : SC092R02 - KEY TYPE MAINTENANCE                            *
     *                                                                        *
     * CREATED   : 01/25/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR095                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * 07/02/2002 C:087 PROJECT: DR176                                        *
     *            There was a paging error that was due to the hidden fields  *
     *            SCKTYP and SCDESC being in SCR01C instead of SCR01S.  I     *
     *            moved the fields to remedy. (no code in this pgm was        *
     *            actually changed)                                           *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * FILE USAGE:                                                            *
     *                                                                        *
     *  File    Description                    Rcdfmt  Key                    *
     *  SMKTL   DARTS Key Type File             PKT01  (KTYP)                 *
     *                                                                        *
     *======================================================================= *
     * INDICATOR USAGE:                                                       *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *  90 - General Error indicator.                                         *
      *  91 - Image product installed.
     *                                                                        *
     *======================================================================= *
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     FSC092f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFile(SCR01S:SFRRN1)
     F                                     SFile(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
    
     FSMKTP     If   e             disk
     F                                     Rename(PKT01:PFRCDF)
    
     FSMKTL     If   e           k disk
     F                                     INFDS(DBFDS)
     F                                     Rename(PKT01:DBRCDF)
    
     FSMKTL2    If   e           k disk
     F                                     INFDS(DBFDS2)
     F                                     Rename(PKT01:DBRCDF2)
      /COPY SC092RINC
     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           S             40a
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'SC092R02'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infomx          c                   2
     D viewmx          c                   1
     D sf1pag          c                   12
     *================================================================*
     * Stand Alone Fields                                             *
     *================================================================*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)

     D                 DS
     D xxData1                       70a
     D  xxKeyTyp               2      3a
     D  xxDesc                10     70a

     D                 DS
     D xxData2                       70a
     D  xxIndex                1     20a
     D  xxIndexPgm            23     70a

     D ##co#           s              3  0
     D ##conm          s             40
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##view          s              1

     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D @@View          s              1
     d xView           S              1a

     D kkktyp          s                   like(ktktyp)
     D kkdesc          s                   like(ktdesc)

     D Deleted         C                   '** DELETED'

     C/COPY SCOPYWW01C
    
     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        PList
     C                   Parm                    @@call
     C                   Parm      *inkc         @@kc
     C                   Parm      *inkl         @@kl
     C                   Parm                    @@rrn
     C                   Parm                    @@view
    
     ** Add/change/display program parameter list
     C     acdpl         PList
     C     *inkc         Parm                    ##kc
     C     *inkl         Parm                    ##kl
     C                   Parm                    ##mode
     C                   Parm      scdbrr        ##rrn
     C                   Parm      xView         ##view
    
     ** Company file retrieve program parameter list
     C     compl         PList
     C                   Parm                    ##co#
     C                   Parm                    ##conm
     *================================================================
     *  KLIST definitions
     *================================================================
     C     key1          KList
     C                   KFld                    kkktyp
    
     C     key2          KList
     C                   KFld                    kkdesc
    
     *================================================================
     * CHKSSM - Set indicator 11 for subset message
     *================================================================
     C     chkssm        BEGSR
     C                   ENDSR
     *================================================================
     * ERRCK1 - Additional errors on subfile selection
     *================================================================
     C     errck1        BEGSR
     C                   ENDSR
     *================================================================
     * ERRCK2 - Error checking for the subset screen
     *================================================================
     C     errck2        BEGSR
     C                   ENDSR
     *================================================================
     * INZSST - Initialize the subset fields
     *================================================================
     C     inzsst        BEGSR
     C                   ENDSR
     *================================================================
     * KEYINZ - Initialize key values for program start
     *================================================================
     C     keyinz        BEGSR
     C                   Eval      kkktyp = *loval
     C                   Eval      kkdesc = *loval
     C                   ENDSR
     *================================================================
     * KEYSET - Set key values when doing "position to"
     *================================================================
     C     keyset        BEGSR

     C                   Eval      kkktyp = SCPST1
     C                   Eval      kkdesc = SCPST2
     C                   Eval      SCPST1 = *blanks
     C                   Eval      SCPST2 = *blanks

     C                   ENDSR
     *================================================================
     * KEYSFL - Set key values from subfile records for paging
     *================================================================
     C     keysfl        BEGSR

     C                   Eval      kkktyp = scktyp
     C                   Eval      kkdesc = scdesc
     C                   ENDSR
     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        BEGSR

     C                   Eval      SCKTYP = KTKTYP
     C                   Eval      SCDESC = KTDESC
     C                   Eval      xxKeyTyp = KTKTYP
     C                   Eval      xxDesc = KTPDES
     C                   Eval      xxIndex = KTDESC
     C                   Eval      xxIndexPgm = KTINPG

     C                   Select

     C                   When      *in21 = *on
     C                   MoveL     xxData1       SCDATA

     C                   When      *in22 = *on
     C                   MoveL     xxData2       SCDATA

     C                   EndSL

     C                   ENDSR
     *================================================================
     * OTHFKS - Process other function keys
     *================================================================
     C     othfks        BEGSR
     C                   ENDSR
     *================================================================
     * OTHOPT - Process other options
     *================================================================
     C     othopt        BEGSR
     C                   ENDSR
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmEnd        BEGSR
     C                   ENDSR
     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        BEGSR

     C                   If        @@Call = '2'
     C                   SetOn                                        30
     C                   EndIf

     C                   If        %parms > 4
     C                   If        @@View <> *blanks
     C                   Move      @@View        View#
     C                   Eval      xView = @@View
     C                   Eval      optnm = Indexes
     C                   Else
     C                   Eval      View# = 1
     C                   Eval      xView = '1'
     C                   Eval      optnm = Types
     C                   EndIf
     C                   Else
     C                   Eval      optnm = Types
     C                   Eval      xView = '1'
     C                   Eval      View# = 1
     C                   EndIf

     C                   ENDSR
     *================================================================
     * READ   - Read next record from file
     *================================================================
     C     Read          BEGSR

     C                   Select
     C                   When      *in21 = *on
     C                   Read      dbrcdf                                 95

     C                   When      *in22 = *on
     C                   Read      dbrcdf2                                95

     C                   EndSL

     C                   ENDSR
     *================================================================
     * READP  - Read previous record from file
     *================================================================
     C     ReadP         BEGSR

     C                   Select

     C                   When      *in21 = *on
     C                   ReadP     dbrcdf                                 95

     C                   When      *in22 = *on
     C                   ReadP     dbrcdf2                                95

     C                   EndSL

     C                   ENDSR
     *================================================================
     * SETGT  - Set greater than on file
     *================================================================
     C     SetGT         BEGSR

     C                   Select

     C                   When      *in21 = *on
     C     key1          SetGT     dbrcdf                             96

     C                   When      *in22 = *on
     C     key2          SetGT     dbrcdf2                            96

     C                   EndSL

     C                   ENDSR
     *================================================================
     * SETLL  - Set lower limits on file
     *================================================================
     C     SetLL         BEGSR

     C                   Select

     C                   When      *in21 = *on
     C     key1          SetLL     dbrcdf                             96

     C                   When      *in22 = *on
     C     key2          SetLL     dbrcdf2                            96

     C                   EndSL

     C                   ENDSR
     *================================================================
     * SF1UPD - Update subfile record when item is deleted
     *================================================================
     C     sf1upd        BEGSR

     C                   MoveL     Deleted       SCDATA

     C                   ENDSR
     *================================================================
     * SLCTRD - Move fields for selected record to parameters
     *================================================================
     C     slctrd        BEGSR

     C                   Eval      @@rrn = SCDBRR

     C                   ENDSR
     *================================================================
     * SUBSET - Determine If record should be put in subfile
     *================================================================
     C     subset        BEGSR
     C                   ENDSR
** Column headings
Opt Key Type  Description
Opt  Index                 Program
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               NNN
3
