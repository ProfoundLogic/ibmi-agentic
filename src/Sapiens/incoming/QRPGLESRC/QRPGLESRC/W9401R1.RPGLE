     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H* ============================================================= *
    H/title W9401R1 - Retrieves tolib name
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H dftactgrp(*no) actgrp(*caller)
    H* ============================================================= *
     H*                                                               *
     H*  DESCRIPTION:  This program retrieves to library name from    *
     H*                a table with from file as an entry parameter   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W9401C/W9402C                                              *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    None.                                                      *
     H*                                                               *
     H*  CREATED: 01/19/10  PROGRAMMER: C:198  PROJECT: 132WJL        *
     H*                                                               *
     H* ============================================================= *
     H*  Revisions:                                                   *
     H*                                                               *
     H*  06/22/15  C:33  Project: 132WQZ                              *
     H*                  Remove 932 hardcoding (Rewrite)              *
     H* ============================================================= *
     D aryLib          s             10    dim(20) ctdata perrcd(1)
     D inFrLib         s             10
     D outToLib        s             10
     D inToSuffix      s              3
     D i               s              3  0
     C     *entry        plist
     C                   parm                    inFrLib
     C                   parm                    outToLib
     C                   parm                    inToSuffix

      /Free
       Clear outToLib;
       Select;
       When %lookup(%subst(inFrLib:1:6):aryLib) <> 0;
          outToLib = %subst(inFrLib:1:6) + inToSuffix;
       When %lookup(%subst(inFrLib:1:7):aryLib) <> 0;
          outToLib = %subst(inFrLib:1:7) + inToSuffix;
       Endsl;
       *inlr = *on;
       Return;
      /End-free
** aryLib
PDSTST    PDSTSTxxx
PDSOBJ    PDSOBJxxx
PDSMOBJ   PDSMOBJxxx
PDSFIL    PDSFILxxx
PDSMFIL   PDSMFILxxx
PDSMTST   PDSMTSTxxx
PDSCLT    PDSCLTxxx
PDSRPT    PDSRPTxxx
PDSQTE    PDSQTExxx
PDSMQTE   PDSMQTExxx
PDSMTH    PDSMTHxxx
PDSTEMP   PDSTEMPxxx
