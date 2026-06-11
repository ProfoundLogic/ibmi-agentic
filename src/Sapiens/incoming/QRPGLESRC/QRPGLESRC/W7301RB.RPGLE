     H* (c) 2013 StoneRiver, Inc.  All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
    H* ================================================================= *
    H/TITLE - Producer Peformance Report, Batch Group Selection
    H* ================================================================= *
    H*  DESCRIPTION:                                                     *
    H*    Runs Producer Performance Report one group at a time, bypass   *
    H*         any group coded as a TEST Group, or any CO# that does     *
    H*         not match the incoming CO# parameter.                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    W6000C  - Daily Batch                                          *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 04/05/06      C:132     PROJECT: WC1420                 *
    H*                                                                   *
    H* ================================================================= *
    H*  REVISIONS:                                                       *
     H* 01/08/10  C:238  Project: 139WNI
     H*                  Agent Loss Ratio Report for all Groups
    H*                                                                   *
    H* ================================================================= *
    F*
     FWMFNL     if   e           k disk
    F*
     C*** BEG   ADD *** 12/18/09 **************************************
     FWXGSL     UF A E           K DISK    RENAME(PGS01:PGS00)
     C*** END   ADD *** 12/18/09 **************************************
    D* ================================================================= *
    D* Stand Alone Fields - TOP
    D* ================================================================= *
     D xxco#           s              3  0
     D xxfnd           s              3
     D @@co#           s              3
     D @@Date          s              8
    D*
    C*================================================================== *
    C*  PLIST definitions
    C*================================================================== *
    C*
     C     *entry        plist
     C                   parm                    @@CO#
     C                   parm                    @@DATE
    C*
    C*================================================================== *
    C*  KLIST definitions
    C*================================================================== *
    C*
     C     FNky          klist
     C                   kfld                    XXCO#
    C*
    C*====================================================================
    C* MAIN  - Processing of each Group
    C*====================================================================
    C*
    C* Read each Group in Company file, and process if not a test group
    C*
     C                   Move      @@CO#         XXCO#
     C     fnky          SetLL     PFN01                              60
     C                   dow       NOT *in60
     C     fnky          ReadE     PFN01                                  60
     C                   Move(P)   fnfnd         XXFND
    C*
     C                   IF        FNtest = 'Y' OR *IN60 = *ON                  Test Group
     C                             OR FNCO# <> XXCO#                            One CO# only
     C                   ITER
     C                   EndIF
    C*
     C*** BEG   DEL *** 01/08/10 **************************************
     C**                 call      'W7301C'
     C**                 PARM      @@CO#         ##CO#             3
     C**                 PARM      XXFND         ##FND             3
     C**                 PARM      '01011950'    ##FRDT            8
     C**                 PARM      '12312099'    ##TODT            8
     C**                 PARM      @@DATE        ##DATE            8
     C**                 PARM      'PRTFDN'      ##OUTQ           10
     C**                 PARM      'B'           ##MODE            1
     C**                 PARM      'N'           ##LR              1
     C**                 PARM      'Y'           ##AGPR            1
     C**                 PARM      '1'           ##SORT            1
     C**                 PARM      'F'           ##OUTP            1
     C**                 PARM      *ZEROS        ##AGT#            5
     C**                 PARM      *ZEROS        ##PROD            5
     C*** END   DEL *** 01/08/10 **************************************
    C*
     C*** BEG   ADD *** 01/08/10 **************************************
     C                   Z-ADD     FNCO#         GSCO#
     C                   Z-ADD     FNFND         GSFND
     C                   MOVEL     FNNAME        GSNAME
     C                   MOVEL     'I'           GSIEFL
     C                   Z-ADD     *zeros        GSRRE#
     C                   WRITE     PGS00
     C*** END   ADD *** 01/08/10 **************************************
     C                   EndDO
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C                   move      *on           *INLR
    C*
    C*====================================================================
