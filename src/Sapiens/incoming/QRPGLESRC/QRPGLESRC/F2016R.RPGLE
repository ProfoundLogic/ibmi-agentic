     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY316 ******** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
      * *****************************************************************
      /TITLE F2016R - SERVICE CODE SELECTION
      * *****************************************************************
      *  Description:  THIS PROGRAM WILL DISPLAY A WINDOW TO ALLOW      *
      *                THE USER TO SELECT WHETHER TO GET THE SERVICE
      *                CODE FROM THE SERVICE CODE FILE FOR THE STATE
      *                OR THE DRG CODE FILE FOR THE STATE.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:  F0363R                                           *
      *                                                                 *
      *  Calls To   :  F2521R - DRG CODE SEARCH                         *
      *                F2320R - SERVICE CODE SEARCH                     *
      *
      *  Created    :   7/13/01  C:79  Project:  114MCG                 *
      *******************************************************************
      * NOTE:                                                           *
      *    @@RTCD = "E" - FAEOP RRN PASSED IN TO THIS PROGRAM.          *
      *    @@RTCD = "C" - PROCEDURE CODE FILE WAS SELECTED.             *
      *    @@RTCD = "H" - HOME HEALTH CODE FILE WAS SELECTED.           *
      *******************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  04/09/18 C:180 Support : PA085                                 *
      *                 Disable DRG, Service code prompts for PA state. *
      *                                                                 *
      *  04/05/17 C:309 Project : NY041                                 *
      *                 Recompile (screen changes only)                 *
      *                                                                 *
      *  10/17/11 C:251 PROJECT 114MAR                                  *
      *                 Added call for CMG file                         *
      *                                                                 *
      *  10/06/11 C:251 PROJECT 114MZO                                  *
      *                 Added call for SNF file                         *
      *                                                                 *
      *  07/15/10 C:251 SUPPORT 72899                                   *
      *                 Allow subset on SMSA from provider file         *
      *                                                                 *
      *  04/19/10 C:227 PROJECT NY020                                   *
      *                 ADDED CALL TO F2541R BASED ON EUDCDT            *
      *                                                                 *
      *  08/31/09 C:227 PROJECT CT005                                   *
      *                 ADDED CHECK FOR CT STATE                        *
      *                                                                 *
      *  07/22/09 C:227 PROJECT AR038                                   *
      *                 ADDED CHECK FOR AR STATE                        *
      *                                                                 *
     F*  03/18/09 C:198 PROJECT TX051                                   *
     F*                 ADDED 2 NEW FIELDS ON WDW01                     *
     F*                                                                 *
      *  06/10/08 C:198 PROJECT TX040                                   *
      *                 Added Home health code file on pressing F10 at  *
      *                 HCPC code.                                      *
      *                                                                 *
      *  09/07/04 C:79 PROJECT NY009                                    *
      *                Use FTDGPNY if state = NY                        *
      *                                                                 *
      *  04/07/04 C:79 Project: AL051                                   *
      *                Add entry parm to determine mode of selection    *
      *                                                                 *
      *  11/15/02 C:79 Project: FSMNT                                   *
      *                Use correct override statement for FTFEL         *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    99 - General File Access                                     *
      * *****************************************************************
     FF2016F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
      * Service Code file
     FFMSVP     IF   E             DISK    USROPN
      * Service Code file
     FFMSVL1    IF   E           K DISK    USROPN
     F                                     RENAME(PSV01:PSV02)
      * Service Code file
     FFMSVL2    IF   E           K DISK    USROPN
     F                                     RENAME(PSV01:PSV03)
      * DRG Code file
     FFTFEP     IF   E             DISK    USROPN
      * DRG Code file
     FFTFEL     IF   E           K DISK    USROPN
     F                                     RENAME(PFE01:PFE02)
      * CPT Code file
     FFMPRP     IF   E             DISK    USROPN
      * CPT Code file
     FFMPRL     IF   E           K DISK    USROPN
     F                                     RENAME(PPR01:PPR02)
     FFTDGPNY   IF   E             DISK
      *
     FFMHHP     IF   E             DISK
     FFAEOP     IF   E             DISK
     FFTAPP     IF   E             DISK
     FFMRCP     IF   E             DISK
     FFAEUL     IF   E           K DISK
     F* DRG Code file
     FFTADPNY   IF   E             DISK
     FFMHPP     IF   E             DISK
     FSTSTL1    IF   E           K DISK
     FFMMGP     IF   E             DISK
     D QCA             S             56    DIM(14) CTDATA PERRCD(1)             CMD EXEC/QCMDEXC
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D                 DS
     D  @@CMD                  1     56
     D  @@ST                  38     39
      *
     D                 DS
     D  @@CMD2                 1     56
     D  @@ST2                 40     41
     D                 DS
     D  @@SVCD                 1     11
     D  XXCDE#                 4      8
     D*** Beg Add **** 07/26/12**************************************
     DRtv_Opps_Apc     pr                  extpgm('F2352R')
     D incall                         1
     D inkc                           1
     D inkl                           1
     D inrrn                          9  0
     D*
     DSrv_code_API     pr                  extpgm('F2320R')
     D incall                         1
     D inkc                           1
     D inkl                           1
     D inst                           2
     D insvcd                        15
     D indesc                        40
     D*
     DRunCommand       pr                  extPgm('QCMDEXC')
     D inCmd                       2750    const
     D inCmdLen                      15  5 const
     D*
     D xxflg           s              1
     D incall          s              1
     D inkc            s              1
     D inkl            s              1
     D inrrn           s              9  0
     D inCmd           s           2750
     D inCmdLen        s             15  5
     D inst            s              2
     D insvcd          s             15
     D indesc          s             40
     D*** Beg Add **** 07/26/12**************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@PROC           11
     C                   PARM                    @@PROD           50
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@AST             2
     C                   PARM                    @@RRN             9 0
     C*
     C*  SERVICE CODE FILE
     C*
     C     SVKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXCPCD                         ##SVCD
     C                   KFLD                    KKEFDT
     C*
     C     SVKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDESC
     C*
     C*  DRG CODE FILE
     C*
     C     DRKY          KLIST
     C                   KFLD                    KKDRG#
     C*
     C*  CPT CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
      *
     C     EUKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
      *
     C     *LIKE         DEFINE    SVCMSV        XXCPCD
     C     *LIKE         DEFINE    SVDESC        KKDESC
     C     *LIKE         DEFINE    SVEFDT        KKEFDT
     C     *LIKE         DEFINE    SVCO#         KKCO#
     C     *LIKE         DEFINE    FEDRG#        KKDRG#
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    @@ST          SCSVST
      *
     C                   EXSR      PGMSTR
      *
      * ---------------------------------------------------------------
      *
     C     *INKL         DOUEQ     *ON
      *
     C     ##SVCD        IFNE      *BLANK
     C     ##RRN         ORNE      *ZERO
     C                   LEAVE
     C                   END
     C     @@AST         IFEQ      'AR'
     C                   MOVEL     '1'           SCCPF
     C                   ELSE
     C                   MOVEL     '1'           SCSVOF
     C                   ENDIF
     C*** Beg Add *** 07/26/12 ****************************************
      /Free
         If @@ast = 'TN';
            scapc  = '1';
            xxflg  = *blanks;
            Dow xxflg = *blanks;
            exfmt wdw01;
            *in71 = *off;
            *in72 = *off;
            *in76 = *off;

            Select;
            When scapc =  '1' and scsvf = '1';
            *in71 = *on;
            xxflg = *blanks;

            When scapc <> '1' and scapc <> *blanks;
            *in76 = *on;
            xxflg = *blanks;

            When scsvf <> '1' and scsvf <> *blanks;
            *in72 = *on;
            xxflg = *blanks;

            Other;
            xxflg = 'Y';
            Endsl;
            Enddo;

            If xxflg = 'Y' and *inkl <> '1';

               Select;
               When scapc = '1';
               incall = '2';
               inkc = *blanks;
               inkl = *blanks;
               inrrn = 0;

               callp Rtv_Opps_Apc(incall:inkc:inkl:inrrn);
               If inrrn <> 0;
               Chain inrrn pap01;
                  If %found(ftapp);
                     xxcde# = aphcpc;
                     @@prod = apdesc;
                     @@proc = @@svcd;
                     @@rtcd = 'C';
                   Else;
                     *inkl = *on;
                  Endif;
               Endif;

               When scsvf = '1';
                 If opnflg = '1';
                    close fmsvp;
                    close fmsvl1;
                    close fmsvl2;
                    opnflg = *blanks;
                 Endif;

                 If ovrflg = '1';
                    incmdlen = 56;

                    inCmd = QCA(1);
                    callp RunCommand(inCmd:inCmdLen);

                    inCmd = QCA(3);
                    callp RunCommand(inCmd:inCmdLen);

                    inCmd = QCA(5);
                    callp RunCommand(inCmd:inCmdLen);
                    ovrflg = *blanks;
                 Endif;

                 If ovrflg = *blanks;
                    incmdlen = 56;

                    @@cmd = qca(2);
                    @@st = scsvst;
                    inCmd = @@cmd;
                    callp RunCommand(inCmd:inCmdLen);

                    @@cmd2= qca(4);
                    @@st2= scsvst;
                    inCmd = @@cmd2;
                    callp RunCommand(inCmd:inCmdLen);

                    @@cmd2= qca(6);
                    @@st2= scsvst;
                    inCmd = @@cmd2;
                    callp RunCommand(inCmd:inCmdLen);
                    ovrflg = '1';
                 Endif;

                 If opnflg <> '1';
                    open fmsvp;
                    open fmsvl1;
                    open fmsvl2;
                    opnflg = '1';
                 Endif;

                 insvcd = *blanks;
                 If @@proc <> *blanks;
                    insvcd = @@proc;
                 Endif;
                  incall = '2';
                  inkc = '0';
                  inkl = '0';
                  inst = scsvst;
                  indesc= *blanks;

                  callp Srv_code_API(incall:inkc:inkl:inst:insvcd:indesc);
                  If insvcd = *blanks and *inkl = *off;
                     *inkl = *on;
                  Endif;
               Endsl;
            Endif;

            If insvcd <> *blanks;
               @@proc = insvcd;
               @@prod = indesc;
            Endif;
            *inkl = *on;
         Else;
      /End-Free
     C*** End Add *** 07/26/12 ****************************************
      *
     C                   EXFMT     WDW01
     C                   MOVEA     '0000000'     *IN(71)
      *
     C     *INKL         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     SCSVOF        WHENEQ    '1'
     C     SCDRF         ANDEQ     '1'
     C     SCSVOF        OREQ      '1'
     C     SCCPF         ANDEQ     '1'
     C     SCSVOF        OREQ      '1'
     C     SCHHF         ANDEQ     '1'
     C     SCSVOF        OREQ      '1'
     C     SCAPF         ANDEQ     '1'
     C     SCSVOF        OREQ      '1'
     C     SCRCF         ANDEQ     '1'
     C     SCSVOF        OREQ      '1'
     C     SCHPS         ANDEQ     '1'
     C     SCSVOF        OREQ      '1'
     C     SCCMG         ANDEQ     '1'
     C     SCDRF         OREQ      '1'
     C     SCCPF         ANDEQ     '1'
     C     SCDRF         OREQ      '1'
     C     SCHHF         ANDEQ     '1'
     C     SCDRF         OREQ      '1'
     C     SCAPF         ANDEQ     '1'
     C     SCDRF         OREQ      '1'
     C     SCRCF         ANDEQ     '1'
     C     SCDRF         OREQ      '1'
     C     SCHPS         ANDEQ     '1'
     C     SCDRF         OREQ      '1'
     C     SCCMG         ANDEQ     '1'
     C     SCCPF         OREQ      '1'
     C     SCHHF         ANDEQ     '1'
     C     SCCPF         OREQ      '1'
     C     SCAPF         ANDEQ     '1'
     C     SCCPF         OREQ      '1'
     C     SCRCF         ANDEQ     '1'
     C     SCCPF         OREQ      '1'
     C     SCHPS         ANDEQ     '1'
     C     SCCPF         OREQ      '1'
     C     SCCMG         ANDEQ     '1'
     C     SCAPF         OREQ      '1'
     C     SCRCF         ANDEQ     '1'
     C     SCAPF         OREQ      '1'
     C     SCHPS         ANDEQ     '1'
     C     SCHHF         OREQ      '1'
     C     SCHPS         ANDEQ     '1'
     C     SCAPF         OREQ      '1'
     C     SCCMG         ANDEQ     '1'
     C     SCHHF         OREQ      '1'
     C     SCCMG         ANDEQ     '1'
     C     SCRCF         OREQ      '1'
     C     SCCMG         ANDEQ     '1'
     C     SCHPS         OREQ      '1'
     C     SCCMG         ANDEQ     '1'
      *
     C                   MOVE      *ON           *IN71                          SELECTED TWO
      *
     C     SCSVOF        WHENNE    '1'
     C     SCSVOF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN72                          INVALID CODE
      *
     C     SCDRF         WHENNE    '1'
     C     SCDRF         ANDNE     *BLANK
     C                   MOVE      *ON           *IN73                          INVALID CODE
      *
     C     SCCPF         WHENNE    '1'
     C     SCCPF         ANDNE     *BLANK
     C                   MOVE      *ON           *IN74                          INVALID CODE
      *
     C     SCAPF         WHENNE    '1'
     C     SCAPF         ANDNE     *BLANK
     C                   MOVE      *ON           *IN76                          INVALID CODE
      *
     C     SCRCF         WHENNE    '1'
     C     SCRCF         ANDNE     *BLANK
     C                   MOVE      *ON           *IN77                          INVALID CODE
      *
     C     SCHHF         WHENNE    '1'
     C     SCHHF         ANDNE     *BLANK
     C                   MOVE      *ON           *IN75                          INVALID CODE
      *
     C     SCHPS         WHENNE    '1'
     C     SCHPS         ANDNE     *BLANK
     C                   MOVE      *ON           *IN78                          INVALID CODE
     C*
     C     SCCMG         WHENNE    '1'
     C     SCCMG         ANDNE     *BLANK
     C                   MOVE      *ON           *IN81                          INVALID CODE
     C*
      * SERVICE CODES
      *
     C     SCSVOF        WHENEQ    '1'
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMSVP
     C                   CLOSE     FMSVL1
     C                   CLOSE     FMSVL2
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*   DELETE OVERRIDE TO FMSVP, FMSVL1, FMSVL2
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH
     C*
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH
     C*
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C     OVRFLG        IFEQ      ' '
     C*
     C*   OVERRIDE TO FMSVP, FMSVL1, FMSVL2
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     SCSVST        @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(4)        @@CMD2
     C                   MOVEL     SCSVST        @@ST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD2
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(6)        @@CMD2
     C                   MOVEL     SCSVST        @@ST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD2
     C                   PARM                    LENGTH
     C*
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMSVP
     C                   OPEN      FMSVL1
     C                   OPEN      FMSVL2
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
     C*
     C                   MOVE      *BLANKS       ##SVCD
     C     @@PROC        IFNE      *BLANKS
     C                   MOVEL(P)  @@PROC        ##SVCD
     C                   ENDIF
     C*
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCSVST        ##ST              2
     C                   PARM                    ##SVCD           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C                   MOVE      *BLANK        SCSVOF
     C*
     C     ##SVCD        IFEQ      *BLANKS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
      *
      * DRG CODES
      *
     C     SCDRF         WHENEQ    '1'
     C     @@AST         ANDNE     'NY'
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FTFEP
     C                   CLOSE     FTFEL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*   DELETE OVERRIDE TO FTFEP, FTFEL
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH
     C*
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C     OVRFLG        IFEQ      ' '
     C*
     C*   OVERRIDE TO FTFEP, FTFEL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(8)        @@CMD
     C                   MOVEL     SCSVST        @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(10)       @@CMD
     C                   MOVEL     SCSVST        @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FTFEP
     C                   OPEN      FTFEL
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
      *
      * DRG SEARCH
      *
     C                   CALL      'F2521R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@AST         ##ST              2
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C                   MOVE      *BLANKS       SCDRF
     C     ##RRN         CHAIN     PFE01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  FEDRG#        XXCDE#
     C                   MOVEL(P)  @@SVCD        @@PROC
     C                   MOVEL(P)  FEDESC        @@PROD
     C                   END
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCDRF
     C*
     C                   ENDIF
      *
      * DRG CODES NEW YORK
      *
     C     SCDRF         WHENEQ    '1'
     C     @@AST         ANDEQ     'NY'
      *
      * DRG SEARCH
      *
     C     EUDCDT        IFGE      1091201
     C                   CALL      'F2541R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@AST         ##ST              2
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZERO
     C     @@AST         ANDEQ     'NY'
     C     ##RRN         CHAIN     PAD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  ADDRG         XXCDE#
     C                   MOVEL(P)  @@SVCD        @@PROC
     C                   MOVEL(P)  ADDESC        @@PROD
     C                   ENDIF                                                  *IN99=*OFF
     C                   ENDIF                                                  ##RRN <> 0
     C                   ELSE                                                   EUDCDT>1091201
     C                   CALL      'F2539R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@AST         ##ST              2
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZERO
     C     @@AST         ANDEQ     'NY'
     C     ##RRN         CHAIN     PDG01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DGDRG         XXCDE#
     C                   MOVEL(P)  @@SVCD        @@PROC
     C                   MOVEL(P)  DGDESC        @@PROD
     C                   END
     C                   END
     C                   ENDIF                                                  EUDCDT>=1091201
     C                   MOVE      *BLANKS       SCDRF
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCDRF
     C*
      *
      * CPT CODES
      *
     C     SCCPF         WHENEQ    '1'
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRP
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*   DELETE OVERRIDE TO FMPRP, FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH
     C*
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C     OVRFLG        IFEQ      ' '
     C*
     C*   OVERRIDE TO FMPRP, FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(8)        @@CMD
     C                   MOVEL     SCSVST        @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     QCA(10)       @@CMD
     C                   MOVEL     SCSVST        @@ST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRP
     C                   OPEN      FMPRL
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
      *
      * CPT SEARCH
      *
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@AST         ##ST              2
     C                   PARM      *BLANKS       ##CPT4           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C                   MOVE      *BLANKS       SCCPF
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCCPF
     C*
     C                   ENDIF
      *
      * CLEAR 'SEARCHING' MSG
     C                   CALL      'SRRMVMSG'                           99
      *
      *
      * MEDICARE APC FILE
      *
     C     SCAPF         WHENEQ    '1'
      *
     C     *IN67         IFEQ      *ON
     C     *IN68         ANDEQ     *ON
      *
     C                   CALL      'F2352R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
      *
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PAP01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     APHCPC        @@PROC
     C                   MOVEL     APDESC        @@PROD
     C                   MOVE      'H'           @@RTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCAPF
     C*
     C                   ENDIF                                                  *IN67 = *ON
      * U & C FILE
      *
     C     SCRCF         WHENEQ    '1'
      *
     C     *IN67         IFEQ      *ON
      *
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
      *
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PRC01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     RCCPT4        @@PROC
     C                   MOVEL     RCDSC1        @@PROD
     C                   MOVE      'H'           @@RTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCRCF
     C*
     C                   ENDIF                                                  *IN67 = *ON
     C     SCHHF         WHENEQ    '1'
     C*
     C                   CALL      'F2546R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PHH01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     HHPRCD        @@PROC
     C                   MOVEL     HHDESC        @@PROD
     C                   MOVE      'H'           @@RTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C*
     C     SCHPS         WHENEQ    '1'
     C** Help from Medicare HIPPS File
     C*
     C                   CALL      'F2376R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PHP01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     HPCODE        @@PROC
     C                   MOVEL     HPDSC1        @@PROD
     C                   MOVE      'H'           @@RTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C*
     C     SCCMG         WHENEQ    '1'
     C** Help from Medicare HIPPS File
     C*
     C                   CALL      'F2384R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PMG01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     MGCODE        @@PROC
     C                   MOVEL     MGDESC        @@PROD
     C                   MOVE      'H'           @@RTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C                   MOVEL     *ON           *INKL
     C*
     C                   ENDIF
     C*
     C                   ENDSL
      *
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C     ##SVCD        IFEQ      *BLANK
     C     ##CPT4        ANDEQ     *BLANK
     C     ##RRN         ANDEQ     *ZERO
     C                   MOVE      '1'           @@RTCD
     C                   END
      *
     C     ##SVCD        IFNE      *BLANK
     C                   MOVEL(P)  ##SVCD        @@PROC
     C                   MOVEL(P)  ##DESC        @@PROD
     C                   END
      *
     C     ##CPT4        IFNE      *BLANK
     C                   MOVEL(P)  ##CPT4        XXCDE#
     C                   MOVEL(P)  @@SVCD        @@PROC
     C                   MOVEL(P)  ##DESC        @@PROD
     C                   MOVE      'C'           @@RTCD
     C                   END
      *
     C*
     C*
     C*** Beg Add *** 07/26/12 ****************************************
      /Free
         Endif;
      /End-Free
     C*** End Add *** 07/26/12 ****************************************
     C                   ENDDO
      *
      *  THE FOLLOWING STMT IS USED TO SATISFY THE COMPILER;
      *  FILES MUST BE REFERENCED BY AN INPUT SPEC.
      *  A CONDITION IS USED TO KEEP FROM ACCESSING THESE
      *  INPUT OPERATIONS, SINCE THESE FILES MAY BE CLOSED.
      *
     C*
     C     *IN99         IFEQ      *ON
     C     *IN99         ANDEQ     *OFF
     C*
     C     ##RRN         CHAIN     PSV01                              99
     C*
     C     SVKY          CHAIN     PSV02                              99
     C*
     C     SVKY1         CHAIN     PSV03                              99
     C*
     C     DRKY          CHAIN     PFE02                              99
     C*
     C     ##RRN         CHAIN     PPR01                              99
     C*
     C     PRKY          CHAIN     PPR02                              99
     C*
     C*
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  GET ADJUDICATION STATE
      *
     C                   MOVEL     @@AST         SCSVST
     C                   MOVEL     Q1PGM         SCPGM
     C                   MOVEL     *BLANKS       @@RTCD
     C     @@AST         IFEQ      'TX'
     C                   SETON                                        67
     C                   ENDIF
     C*** NEED TO TURN ON INDICATOR 68 ON TEH BASIS OF EUICN  LIKE 'AO/AS/HO/HS'
     C     Q1PRMS        IFGT      7
     C     @@RRN         CHAIN     PEO01                              99
     C  N99EUKY          CHAIN     PEU01                              99
     C  N99EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   SETON                                        68
     C                   ENDIF                                                  EUICN = 'AO/AS/HO/HS
     C                   ENDIF                                                  Q1PRMS > 4
     C     @@AST         IFEQ      'AR'
     C                   SETON                                        57
     C                   ENDIF
     C     @@AST         IFEQ      'CT'
     C                   SETON                                        58
     C                   ENDIF
     C* GET THE STMFS FLAG
     C                   MOVEL(P)  EOAST         XXST             10
     C     XXST          CHAIN     STSTL1                             99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STMFS         XXMFS             1
     C                   ENDIF
     C*
     C                   MOVEL     EUBLTY        XXBLTY            2
     C*
     C     XXBLTY        IFEQ      '18'
     C     XXBLTY        OREQ      '21'
     C*
     C     XXMFS         IFEQ      'Y'
     C                   SETON                                        79
     C                   ELSE
     C                   SETOFF                                       79
     C                   ENDIF
     C                   ENDIF
     C     XXMFS         IFEQ      'Y'
     C     EUICN         ANDEQ     'IR'
     C                   SETON                                        80
     C                   ELSE
     C                   SETOFF                                       80
     C                   ENDIF
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    '1'
     C                   SETON                                        50
      *
     C                   OTHER
      *
     C                   ENDSL
     C*** Beg Add *** 07/26/12 *****************************************
      /Free
          If @@AST = 'TN';
             *in69 = *on;
          Else;
             *in69 = *off;
          Endif;

       //*** Beg Add *** 04/09/18 *****************************************
          If @@AST = 'PA';
             *in65 = *on;
          Else;
             *in65 = *off;
          Endif;
       //*** End Add *** 04/09/18 *****************************************

      /End-free
     C*** End Add *** 07/26/12 *****************************************
      *
     C                   ENDSR
**  QCMDEXC TABLE
DLTOVR FILE(FMSVP)
OVRDBF FILE(FMSVP) TOFILE(*LIBL/FMSVP  )
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
DLTOVR FILE(FMSVL2)
OVRDBF FILE(FMSVL2) TOFILE(*LIBL/FMSVL2  )
DLTOVR FILE(FTFEP)
OVRDBF FILE(FTFEP) TOFILE(*LIBL/FTFEP  )
DLTOVR FILE(FTFEL)
OVRDBF FILE(FTFEL) TOFILE(*LIBL/FTFEL  )
DLTOVR FILE(FMPRP)
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
