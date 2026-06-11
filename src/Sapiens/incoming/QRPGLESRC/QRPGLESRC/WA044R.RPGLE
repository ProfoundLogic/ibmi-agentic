     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  WA044R - WCMS Daily batch lockout           C:53 05/31/00    *
     H*                                                               *
     H*  CALLED FROM: W0070R01                                        *
     H*               W0080R                                          *
     H*               W0120R                                          *
     H*               W0122R                                          *
     H*               W0581R                                          *
     H*               W0583R                                          *
     H*               W8080R                                          *
     H*                                                               *
     H*  CALLS TO: none                                               *
     H*****************************************************************
     H*  DESCRIPTION: Checks WDAILYA for a lockout situation.  If the *
     H*               batch is processing then display an error screen*
     H*               and pass back a fail return code.               *
     H*****************************************************************
     H*  CREATED: 05/31/00  C:53 Project WC461                        *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 10/23/12 C:33  Project: WC2116                                *
     H*                Do not display screens for calls from AJAX     *
     H*                                                               *
     H* 09/26/11 C:189 SUPPORT: 76628                                 *
     H*                Added a new parameter to the program which     *
     H*                will help to set the 7th position of daily     *
     H*                batch data area WDAILYA TO 'Y'                 *
     H*                                                               *
     H* 01/22/09 C:33  Project: WC1647  WA114/66866                   *
     H*                Remove message for CLT114 as it interupts CSIs *
     H*                 workflow.                                     *
     H*                                                               *
     H*  05/01/08 C:190 Project: WC1647                               *
     H*                 Add a 1 character parm to the entry list for  *
     H*                 a manual check lockout flag.                  *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FWA044F    CF   E             WORKSTN
     F                                     usropn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*

     D MG1             S             60    DIM(4) CTDATA PERRCD(1)              MSG1
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DLYA                   1     80
     D  DLYA1                  7      7
     D                 DS
     D  SCMA                   1    200
     D                                     DIM(4)                               SCDATA
     D  SCMA01                 1     50
     D  SCMA02                51    100
     D  SCMA03               101    150
     D  SCMA04               151    200
     D                 DS
     D  SCMB                   1    150
     D  SCMB01                 1     50
     D  SCMB02                51    100
     D  SCMB03               101    150
     D*
     D CHGNUM          C                   CONST('SA020R')
     D TITL2           C                   CONST('This is an informa-
     D                                     tion window.')

     C*
     C     *DTAARA       DEFINE    WDAILYA       DLYA
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@MCKL            1
     C                   PARM                    @@UPKL            1
     C*
     C     CHGPL         PLIST
     C                   PARM                    QQNUM            80
     C                   PARM                    QQMASK           10
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '114'
     C                   MOVE      *BLANK        @@RTCD
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDSL
      /Free
       Select;
       When @@rtCd <> '*';
          If not %open(WA044F);
             Open WA044F;
          Endif;
       Endsl;
      /End-free
     C*
     C     *LOCK         IN        DLYA                                 80
     C     *IN80         IFEQ      '1'
     C                   EXSR      MSG1
      /Free
       If %open(WA044F);
      /End-free
     C                   EXFMT     SCR01
      /Free
       Endif;
      /End-free
     C                   MOVE      'F'           @@RTCD
     C                   ELSE
     C                   MOVE      *BLANK        @@RTCD
     C                   END
     C*
     C     Q1PRMS        IFEQ      2
     C     @@RTCD        IFEQ      ' '
     C     @@MCKL        ANDEQ     'Y'
     C     1             SUBST     DLYA:7        XXDLYA            1
     C     XXDLYA        IFEQ      'Y'
     C                   EXSR      MSG1
      /Free
       If %open(WA044F);
      /End-free
     C                   EXFMT     SCR01
      /Free
       Endif;
      /End-free
     C                   MOVE      'F'           @@RTCD
     C                   ELSE
     C                   MOVE      *BLANK        @@RTCD
     C                   ENDIF                                                  XXDLYA = 'Y'
     C                   ENDIF                                                  @@RTCD = ' '
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C* Change the 7th position of data area WDAILYA to 'Y'
     C*
     C     Q1PRMS        IFGT      2
     C     @@UPKL        IFEQ      'Y'
     C     *LOCK         IN        DLYA
     C                   MOVE      'Y'           DLYA1
     C                   OUT       DLYA
     C                   ENDIF                                                  UPKL = 'Y'
     C                   ENDIF                                                  Q1PRMS > 2
      /Free
       If %open(WA044F);
          Close WA044F;
       Endif;
      /End-free
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C*=====================================================
     C     MSG1          BEGSR
     C*=====================================================
     C                   DO        4             I                 2 0
     C                   MOVEA     MG1(I)        SCMA(I)
     C                   ENDDO
     C                   ENDSR
     C*
** MG1                                           |

  Access to this program is denied because the
  Daily Batch is processing.  Please signoff or
  contact your IS manager.
