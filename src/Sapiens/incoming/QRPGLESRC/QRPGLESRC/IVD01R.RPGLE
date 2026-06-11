     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IVD01R  - ACCESSING VISUAL DARTS INFO DISPLAY            *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*  PROGRAMMER: 02                                               *
     H*  CREATED:     9/09/99             PROJECT:                    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 08/03/13  C:33  Implement GS (no comments)
     H*                                                               *
     H* 10/12/07 C:65 SUPPORT 59694                                   *
     H*               CHANGE VISUAL DARTS TO IDARTS.                  *
     H*                                                               *
     H* 10/28/04 C:62 PROJECT 44243                                   *
     H*               REMOVE WAIT WINDOW                              *
     H*                                                               *
     H*****************************************************************
     H*  FILE USGE:                                                  *
     H*****************************************************************
     F*
     FIVD01F    CF   E             WORKSTN usrOpn
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIVD01FGS  CF   E             WORKSTN usrOpn
     F                                     INFDS(FIDS01G)

     D*
     D HEAD            C                   CONST('Accessing Darts Int-
     D                                     erface')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D FIDS01G       E DS                  EXTNAME(SZQ2P)
     D                                     prefix(QG:2)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     DisUseGS          s              1n   inz

     D/copy sPrAtrium

      /Free
       isUseGS = not AtriumJob;

       Select;
       When isUseGS;
          Open IVD01FGS;
       Other;
          Open IVD01F;
       Endsl;
      /End-free

     C     *ENTRY        PLIST
     C                   PARM                    QQRTCD            1
     C*
     C                   MOVEL     HEAD          SCPRD3
     C                   MOVEL     Q1USER        SCUSER
     C*
     C                   MOVE      *OFF          *INKC
     C*
     C     QQRTCD        CABEQ     *BLANK        SCR01T
     C     QQRTCD        CABEQ     '1'           SCR02T
     C     QQRTCD        CABEQ     '2'           SCR03T
     C     QQRTCD        CABEQ     '*'           SCR99T
     C*
     C     SCR01T        TAG
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      ' '           QQRTCD
     C                   GOTO      DONE
     C*
     C     SCR02T        TAG
     C                   MOVE      *BLANK        QQRTCD
      /Free
       Select;
       When isUseGS;
          Exfmt scr02G;
       Other;
          Exfmt scr02;
       Endsl;
      /End-free
     C                   GOTO      DONE
     C*
     C     SCR03T        TAG
     C                   MOVE      *BLANK        QQRTCD
      /Free
       Select;
       When isUseGS;
          Exfmt scr03G;
       Other;
          Exfmt scr03;
       Endsl;
      /End-free
     C                   GOTO      DONE
     C*
     C     SCR99T        TAG
     C                   MOVE      *BLANK        QQRTCD
      /Free
       Select;
       When isUseGS;
          Write scr99G;
       Other;
          Write scr99;
       Endsl;
      /End-free
     C                   WRITE     SCR99
     C*
     C     DONE          TAG
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
