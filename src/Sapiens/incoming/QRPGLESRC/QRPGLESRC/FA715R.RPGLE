     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA715R - API TO RETREIVE FMCXP SECURITY AND REMARK CODES
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*  Called From:                                                   *
     H*    VARIOUS PROGRAMS                                             *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  09/14/98  C:62  Project:  MC145                      *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*
     H*  05/07/99 C:56 Project: MC152
     H*                Add get for following control info
     H*                CXRCAL - EOMB Recon allowed flag
     H*                CXRCSL - EOMB Recon Security Level
     H*                CXRCST - EOMB Recon Security Type
     H*
     H*  03/15/99 C:56 PROJECT:MC152
     H*                ADD EOMB WITH CHECK ATTACHED SECURITY LEVEL
     H*                AND TYPE
     H*
     H*  01/07/99 C:62 PROJECT: 25MKK
     H*                ADDED NEW PARMS FOR CXPLCL AND CXPLCT.
     H*
     H*  01/04/99 C:62 PROJECT: 25MKK
     H*                ADDED NEW PARMS FOR CXNWPA, CXCPAA AND CXCPAD.
     H*
     H*  11/20/98 C:62 PROJECT: MC145
     H*                ADDED NEW PARMS FOR CXCS#L, CXCS#T, CXNERC,
     H*                CXMEUS, CXDNSL AND CXDNST.
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    99 - General File Access                                     *
     H*******************************************************************
     F*
     FFMCXL     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
P01  C                   PARM                    @@RTCD            1
P02  C                   PARM                    @@CO#             3 0
P03  C                   PARM                    @@SUB#            5 0
P04  C                   PARM                    @@FND             3 0
P05  C                   PARM      CXPVMC        @@PVMC            1 0          Init Med
P06  C                   PARM      CXPVPR        @@PVPR            1 0          Init Prov
P07  C                   PARM      CXWCUP        @@WCUP            1            Allw Upd WCMS Clms
P08  C                   PARM      CXCLDL        @@CLDL            3 0          Clm Dlt Lvl
P09  C                   PARM      CXCLDT        @@CLDT            1            Clm Dlt Typ
P10  C                   PARM      CXCLTL        @@CLTL            3 0          Clm Xfer Lvl
P11  C                   PARM      CXCLTT        @@CLTT            1            Clm Xfer Typ
P12  C                   PARM      CXSDCL        @@SDCL            3 0          Settle Dt Lvl
P13  C                   PARM      CXSDCT        @@SDCT            1            Settle Dt Typ
P14  C                   PARM      CXBSCL        @@BSCL            3 0          BWC Sts Lvl
P15  C                   PARM      CXBSCT        @@BSCT            1            BWC Sts Typ
P16  C                   PARM      CXDCDL        @@DCDL            3 0          Del Diag Cd Lvl
P17  C                   PARM      CXDCDT        @@DCDT            1            Del Diag Cd Typ
P18  C                   PARM      CXDCCL        @@DCCL            3 0          Chg Diag St Lvl
P19  C                   PARM      CXDCCT        @@DCCT            1            Chg Diag St Typ
P20  C                   PARM      CXC#CL        @@C#CL            3 0          Clt Case Lvl
P21  C                   PARM      CXC#CT        @@C#CT            1            Clt Case Typ
P22  C                   PARM      CXFIMR        @@FIMR            5            RMCD: From IMR
P23  C                   PARM      CXTIMR        @@TIMR            5            RMCD: To IMR
P24  C                   PARM      CXFRN         @@FRN             5            RMCD: From RN
P25  C                   PARM      CXTRN         @@TRN             5            RMCD: To RN
     C*** BEGIN ADD *** 11/20/98 *************************************
P26  C                   PARM                    @@CS#L            3 0          St Clm#  Lvl
P27  C                   PARM                    @@CS#T            1            St Clm#  Typ
P28  C                   PARM                    @@DNSL            3 0          Ovr DNYD Lvl
P29  C                   PARM                    @@DNST            1            Ovr DNYD Typ
P30  C                   PARM                    @@NERC            5            RMCD: New Emplr
P31  C                   PARM                    @@MEUS           10            New Employer
     C*** END   ADD *** 11/20/98 *************************************
     C*** BEGIN ADD *** 01/04/99 *************************************
P32  C                   PARM                    @@NWPA            5            RMCD: ADD PC
P33  C                   PARM                    @@CPAA            5            RMCD: 'A' OR 'N'
P34  C                   PARM                    @@CPAD            5            RMCD: 'D'
     C*** END   ADD *** 01/04/99 *************************************
     C*** BEGIN ADD *** 01/04/99 *************************************
P35  C                   PARM                    @@PLCL            3 0          LIAB CHG LVL
P36  C                   PARM                    @@PLCT            1            LIAB CHG TYP
     C*** END   ADD *** 01/04/99 *************************************
     C*
     C*** BEG ADD *** 03/15/99 ****************************************
P37  C                   PARM                    @@CKDL            3 0          EOMB DEL W/CHK LVL
P38  C                   PARM                    @@CKDT            1            EOMB DEL W/CHK TYP
     C*** END ADD *** 03/15/99 ****************************************
     C*
     C*** BEG ADD *** 05/07/99 ****************************************
P39  C                   PARM                    @@RCAL            1            EOMB RECON ALLOWED
P40  C                   PARM                    @@RCSL            3 0          EOMB RECON SEC LEVEL
P41  C                   PARM                    @@RCST            1            EOMB RECON SEC TYPE
     C*** END ADD *** 05/07/99 ****************************************
     C*
     C     CXKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C*
     C     CXKY          CHAIN     PCX01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C*** BEGIN ADD *** 11/20/98 *************************************
     C                   ELSE
     C*
     C     Q1PRMS        IFGT      25
     C*
     C                   Z-ADD     CXCS#L        @@CS#L
     C                   MOVEL(P)  CXCS#T        @@CS#T
     C                   Z-ADD     CXDNSL        @@DNSL            3 0          Ovr DNYD Lvl
     C                   MOVEL(P)  CXDNST        @@DNST            1            Ovr DNYD Typ
     C                   MOVEL(P)  CXNERC        @@NERC            5            RMCD: New Emplr
     C                   MOVEL(P)  CXMEUS        @@MEUS           10            New Employer
     C*
     C                   ENDIF
     C*** END   ADD *** 11/20/98 *************************************
     C*
     C*** BEGIN ADD *** 01/04/99 *************************************
     C     Q1PRMS        IFGT      31
     C*
     C                   MOVEL(P)  CXNWPA        @@NWPA                         RMCD: ADD PC
     C                   MOVEL(P)  CXCPAA        @@CPAA                         RMCD: 'A' OR 'N'
     C                   MOVEL(P)  CXCPAD        @@CPAD                         RMCD: 'D'
     C*
     C                   ENDIF
     C*** END   ADD *** 01/04/99 *************************************
     C*
     C*** BEGIN ADD *** 01/07/99 *************************************
     C     Q1PRMS        IFGT      34
     C*
     C                   Z-ADD     CXPLCL        @@PLCL
     C                   MOVEL(P)  CXPLCT        @@PLCT
     C*
     C                   ENDIF
     C*** END   ADD *** 01/07/99 *************************************
     C*
     C*** BEG ADD *** 03/15/99 ****************************************
     C     Q1PRMS        IFGE      38
     C                   Z-ADD     CXCKDL        @@CKDL
     C                   MOVEL(P)  CXCKDT        @@CKDT
     C                   ENDIF                                                  Q1PRMS >= 38
     C*** END ADD *** 03/15/99 ****************************************
     C*
     C*** BEG ADD *** 05/07/99 ****************************************
     C     Q1PRMS        IFGE      41
     C                   MOVEL     CXRCAL        @@RCAL
     C                   Z-ADD     CXRCSL        @@RCSL
     C                   MOVEL     CXRCST        @@RCST
     C                   ENDIF                                                  Q1PRMS >= 41
     C*** END ADD *** 05/07/99 ****************************************
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
