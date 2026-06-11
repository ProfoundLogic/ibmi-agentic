     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE API - FA783R - Florida: Retrieve EOB Description         *
     H*****************************************************************
     H* Description:  This program will be used to Retrieve EOB code  *
     H*               Description.                                    *
     H*                                                               *
     H*  Called From: F0340R, N96DAR, N96DTR, N96DUR, N96DVR, N96DWR  *
     H*                                                               *
     H*  Calls To:    None                                            *
     H*                                                               *
     H*****************************************************************
     H* Created on 04/30/2013 by C:277   for  Project 147MAS          *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 07/18/23 C:342 Project: NY060                                 *
     H*                NY Compliance: CARC/RARC Update                *
     H*                                                               *
     H*****************************************************************
     FFTABLFL   if   e           k DISK
     D*
     D psds          esds                  extname(szq1p)
     D*
     D                 ds
     DTNote                    1    310
     D EBDESC                  1     70
     D EBDSC2                 71    140
     D EBDSC3                141    210
     D EBDSC4                211    280
     D
     D X1DESC          s            280
     C**** Beg Add *** 07/18/23 **************************************
     D @@EOB           s              6
     C**** End Add *** 07/18/23 **************************************
     D*
     C*================================================================
     C* Parameter lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@EOB
     C                   PARM                    @@TEXT
     C                   PARM                    @@DSC1
     C                   PARM                    @@DSC2
     C                   PARM                    @@DSC3
     C                   PARM                    @@DSC4
     C*================================================================
     C* Key lists
     C*================================================================
     C     EBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*================================================================
     C* Like Defines
     C*================================================================
     C     *like         define    EBCO#         @@CO#
     C**** Beg Del *** 07/18/23 **************************************
     C**** *like         define    EBXREF        @@EOB
     C**** End Del *** 07/18/23 **************************************
     C     *like         define    EBXREF        @@XREF
     C     *like         define    EBDESC        @@DSC1
     C     *like         define    EBDSC2        @@DSC2
     C     *like         define    EBDSC3        @@DSC3
     C     *like         define    EBDSC4        @@DSC4
     C     *like         define    EBDESC        @@TEXT
     C     *like         define    EBDESC        XNOTE
     C     *like         define    EBCO#         KKCO#
     C     *like         define    EBKEY         KKKEY
     C*
     C                   move      *BLANKS       @@DSC1
     C                   move      *BLANKS       @@DSC2
     C                   move      *BLANKS       @@DSC3
     C                   move      *BLANKS       @@DSC4
     C*
     C*                  move      @@text        x1desc
     C                   move      @@CO#         KKCO#
     C                   movel     @@EOB         KKKEY
     C*
     C     EBKY          chain     PEB01                              99
     C*
     C     *in99         ifeq      *OFF
     C*
     C                   eval      x1desc = TNote
     C                   eval      str = 0
     C                   eval      str = %scan('(': x1desc)
     C                   if        str > 0
     C                   eval      x1desc = %subst(x1desc: 1 : str - 1)
     C                   endif
     C                   eval      x1desc = %TRIM(x1desc) + ' ' + @@text
     C
     C                   exsr      SRWRAP
     C*
     C                   endif
     C*
     C                   return
     C*---------------------------------------------------------------------
     C*  SRWRAP : WORD WRAPPING LOGIC SO THAT WORDS DON'T CUT OFF
     C*---------------------------------------------------------------------
     C     SRWRAP        begsr
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   z-add     1             str               3 0
     C                   z-add     0             end               3 0
     C                   z-add     0             n1                3 0
     C                   z-add     1             loop              1 0
     C*
     C     loop          downe     4
     C*
     C     70            add       str           end               3 0
     C*
     C     end           ifge      310
     C*
     C     1             subst     EBDSC4:1      XXBLK
     C                   z-add     310           end
     C*
     C     end           downe     0
     C     XXBLK         ifne      ' '
     C                   sub       1             end
     C                   else
     C                   leave
     C                   endif
     C     1             subst     x1desc:end    XXBLK
     C                   enddo
     C*
     C                   else
     C*
     C     end           downe     0
     C*
     C     1             subst     x1desc:end    XXBLK             1
     C*
     C     XXBLK         ifNE      ' '
     C                   sub       1             END
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   endif
     C*
     C                   clear                   XNOTE
     C     end           sub       STR           n1
     C     n1            ifgt      0
     C     n1            subst     x1desc: str   XNOTE
     C                   else
     C     70            subst     x1desc: str   XNOTE
     C                   endif
     C*
     C** REMOVE LEADING BLANKS FROM THE NOTE
     C     ' '           check     XNOTE         pos                      98
     C     *in98         ifeq      *on
     C     pos           andgt     *zeros
     C*
     C                   clear                   TMP1            140
     C                   movel     XNOTE         TMP1
     C                   clear                   XNOTE
     C*
     C     70            subst     TMP1:POS      XNOTE
     C                   endif
     C*
     C                   select
     C                   when      loop =  1
     C                   movel     XNOTE         @@DSC1
     C                   when      loop = 2
     C                   movel     XNOTE         @@DSC2
     C                   when      loop = 3
     C                   movel     XNOTE         @@DSC3
     C                   endsl
     C*
     C     end           ifgt      0
     C                   z-add     END           str
     C     n1            iflt      70
     C                   add       1             str
     C                   endif
     C                   else
     C                   add       70            str
     C                   endif
     C*
     C                   add       1             loop
     C                   enddo
     C*
     C*** CATER TO LAST DESCRIPTION
     C*
     C                   z-add     0             pos               3 0
     C                   move      *blanks       TMNOT1          140
     C*
     C     ' '           checkr    x1desc:280    pos                      98
     C*
     C     *in98         ifeq      *on
     C     pos           sub       str           pos
     C                   add       1             pos
     C*
     C     pos           ifgt      *zeros
     C     pos           subst     x1desc:str    TMNOT1
     C                   endif
     C                   endif
     C*
     C     ' '           checkr    TMNOT1:140    pos                      98
     C     *in98         ifeq      *on
     C                   cat       EBDSC4:0      TMNOT1
     C                   else
     C                   movel     EBDSC4        TMNOT1
     C                   endif
     C*
     C     70            subst     TMNOT1:1      @@DSC4
     C*
     C** REMOVE LEADING BLANKS FROM THE NOTE
     C*
     C     ' '           check     @@DSC4        pos                      98
     C     *in98         ifeq      *on
     C     pos           andgt     *zeros
     C*
     C                   clear                   TMP1
     C                   movel     @@DSC4        TMP1
     C                   clear                   @@DSC4
     C*
     C     70            subst     TMP1:POS      @@DSC4
     C                   endif
     C*
     C*
     C                   endsr
