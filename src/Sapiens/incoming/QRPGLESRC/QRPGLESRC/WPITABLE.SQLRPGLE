     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 06/17/24  C:344 Project: WC2952                                                    *
     H*                 Increase rate field to zoned 8.3                                   *
     H*                                                                                    *
     H* 05/16/24  C:62  Project: 146WKQ                                                    *
     H*                 Retrieve bill schedule type.                                       *
     H*                                                                                    *
     H* 11/04/20  C:33  Project: SY409                                                     *
     H*                 Include the extention when retrieving class code description       *
     H*                                                                                    *
     H* 09/04/19  C:33  Project: SY408                                                     *
     H*                 Allow generic table routines to accept an optional prefix          *
     H*                                                                                    *
     H* 07/10/19  C:33  Project: SY408                                                     *
     H*                 Allow generic table routines to accept a varying size code         *
     H*                                                                                    *
     H* 07/24/12  C:191 Project: Transform                                                 *
     H*                 Add procedure GetGenCodeDS to get descrition from generic table   *
     H*                                                                                    *
     H* 06/05/12  C:191 Project: Transform                                                 *
     H*                 Add procedure to validate generic table code                      *
     H*                                                                                    *
     H**************************************************************************************
     H nomain
    F*
     Fwtabl     if   e           k disk    infds(tbfds)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     F***  Begin Add  ***  09/04/19  ****************************************
     Fwtgil     if   e           k disk    rename(pgi01:pgi00)
     F***  End   Add  ***  09/04/19  ****************************************

     D tbfds           ds
     D tb_rrn                397    400b 0

     d***  Begin Add  ***  09/04/19  ****************************************
     D/copy sPrString
     D***  End   Add  ***  09/04/19  ****************************************
     D/copy wprtable

     D psds          esds                  extname(szq1p)

     D company         s              3  0
     D tableCode       s             10

     C     tableKy       klist
     C                   kfld                    company
     C                   kfld                    tableCode





    P*====================================================================
    P* AgtTypCdDs - Get the agent type code description
    P*
    P*  AgtTypCdDs - returns the agent type code description
    P*   co#       - accepts the company #       (required)
    P*   code      - accepts the agent type code (required)
    P*   ATrrn     - returns the RRN of WTATP
    P*====================================================================
    P*
     PAgtTypeDes       b                   export
    P*
     DAgtTypeDes       PI            30
     D co#                            3  0 const
     D code                          10    const
     D ATrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   ATDESC
     C     tableKy       chain     pat01                              60
     C                   eval      desc = ATDESC
     C*
     C                   if        %parms >= 3
     C                   eval      ATrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PAgtTypeDes       e





    P*====================================================================
    P* ARBtchCdDs - Get the A/R batch code description
    P*
    P*  ARBtchCdDs - returns the A/R batch code description
    P*   co#       - accepts the company #     (required)
    P*   code      - accepts the A/R batch code (required)
    P*   SYrrn     - returns the RRN of WTSYP
    P*====================================================================
    P*
     PARBtchCdDs       b                   export
    P*
     DARBtchCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D SYrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   SYDESC
     C     tableKy       chain     psy01                              60
     C                   eval      desc = SYDESC
     C*
     C                   if        %parms >= 3
     C                   eval      SYrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PARBtchCdDs       e





    P*====================================================================
    P* ARTranCdDs - Get the A/R transaction code description
    P*
    P*  ARTranCdDs - returns the A/R transaction code description
    P*   co#       - accepts the company #      (required)
    P*   code      - accepts the A/R trans code (required)
    P*   TTrrn     - returns the RRN of WTTTP
    P*====================================================================
    P*
     PARTranCdDs       b                   export
    P*
     DARTranCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D TTrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   TTDESC
     C     tableKy       chain     ptt01                              60
     C                   eval      desc = TTDESC
     C*
     C                   if        %parms >= 3
     C                   eval      TTrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PARTranCdDs       e





    P*====================================================================
    P* BenAdjCdDs - Get the benefit adjustment type code description
    P*
    P*  AgtTypeDes - returns the benefit adjustment type code description
    P*   co#     - accepts the company #               (required)
    P*   code    - accepts the benefit adjustment code (required)
    P*   BArrn   - returns the RRN of WTBAP
    P*====================================================================
    P*
     PBenAdjCdDs       b                   export
    P*
     DBenAdjCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D BArrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   BADESC
     C     tableKy       chain     pba01                              60
     C                   eval      desc = BADESC
     C*
     C                   if        %parms >= 3
     C                   eval      BArrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PBenAdjCdDs       e





    P*====================================================================
    P* BillCmntDs - Get the billing comment code description
    P*
    P*  AgtTypeDes - returns the billing comment code description
    P*   co#       - accepts the company #            (required)
    P*   code      - accepts the billing comment code (required)
    P*   BCrrn     - returns the RRN of WTBCP
    P*====================================================================
    P*
     PBillCmntDs       b                   export
    P*
     DBillCmntDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D BCrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   BCDESC
     C     tableKy       chain     pbc01                              60
     C                   eval      desc = BCDESC
     C*
     C                   if        %parms >= 3
     C                   eval      BCrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PBillCmntDs       e





    P*====================================================================
    P* BillSchdDs - Get the billing schedule code description
    P*
    P*  BillSchdDs - returns the billing schedule code description
    P*   co#       - accepts the company #             (required)
    P*   code      - accepts the billing schedule code (required)
    P*   BSrrn     - returns the RRN of WTBSP
    P*====================================================================
    P*
     PBillSchdDs       b                   export
    P*
     DBillSchdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D BSrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   BSDESC
     C     tableKy       chain     pbs01                              60
     C                   eval      desc = BSDESC
     C*
     C                   if        %parms >= 3
     C                   eval      BSrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PBillSchdDs       e





     p*** Begin Add *** 05/16/24 *******************************************
    P*====================================================================
    P* BillSchdType - Get the billing schedule code type
    P*
    P*  BillSchdType - returns the billing schedule code type (S/E)
    P*   co#       - accepts the company #             (required)
    P*   code      - accepts the billing schedule code (required)
    P*   BSrrn     - returns the RRN of WTBSP
    P*====================================================================
    P*
     PBillSchdType     b                   export
    P*
     DBillSchdType     PI            30
     D co#                            3  0 const
     D code                          10    const
     D BSrrn                          9  0       options(*nopass)

     D type            s              1

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   BSseFl
     C     tableKy       chain     pbs01                              60
     C                   eval      type = BSseFl
     C*
     C                   if        %parms >= 3
     C                   eval      BSrrn = TB_rrn
     C                   endif
     C*
     C                   return    type
     C*
     PBillSchdType     e





     p*** End   Add *** 05/16/24 *******************************************
    P*====================================================================
    P* BR2CodeDs - Get the BR-2 code description
    P*
    P*  BR2CodeDs - returns the BR-2 code description
    P*   co#      - accepts the company #  (required)
    P*   code     - accepts the BR-2 code  (required)
    P*   B2rrn    - returns the RRN of WTB2P
    P*====================================================================
    P*
     PBR2CodeDs        b                   export
    P*
     DBR2CodeDs        PI            30
     D co#                            3  0 const
     D code                          10    const
     D B2rrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   B2DESC
     C     tableKy       chain     pb201                              60
     C                   eval      desc = B2DESC
     C*
     C                   if        %parms >= 3
     C                   eval      B2rrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PBR2CodeDs        e





    P*====================================================================
    P* BR7CodeDs - Get the BR-7 code description
    P*
    P*  BR7CodeDs - returns the BR-7 code description
    P*   co#      - accepts the company #  (required)
    P*   code     - accepts the BR-7 code  (required)
    P*   B7rrn    - returns the RRN of WTB7P
    P*====================================================================
    P*
     PBR7CodeDs        b                   export
    P*
     DBR7CodeDs        PI            30
     D co#                            3  0 const
     D code                          10    const
     D B7rrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   B7DESC
     C     tableKy       chain     pb701                              60
     C                   eval      desc = B7DESC
     C*
     C                   if        %parms >= 3
     C                   eval      B7rrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PBR7CodeDs        e





    P*====================================================================
    P* CauseCdDs  - Get the cause code description
    P*
    P*  CauseCdDs - returns the cause code description
    P*   co#      - accepts the company #  (required)
    P*   code     - accepts the cause code (required)
    P*   CArrn    - returns the RRN of WTCAP
    P*====================================================================
    P*
     PCauseCdDs        b                   export
    P*
     DCauseCdDs        PI            30
     D co#                            3  0 const
     D code                          10    const
     D CArrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   CADESC
     C     tableKy       chain     pca01                              60
     C                   eval      desc = CADESC
     C*
     C                   if        %parms >= 3
     C                   eval      CArrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PCauseCdDs        e





    P*====================================================================
    P* ChkCtlCdDs - Get the check control code description
    P*
    P*  ChkCtlCdDs - returns the check control code description
    P*   co#       - accepts the company #     (required)
    P*   code      - accepts the rate set code (required)
    P*   CCrrn     - returns the RRN of WTCCP
    P*====================================================================
    P*
     PChkCtlCdDs       b                   export
    P*
     DChkCtlCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D CCrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   CCDESC
     C     tableKy       chain     pcc01                              60
     C                   eval      desc = CCDESC
     C*
     C                   if        %parms >= 3
     C                   eval      CCrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PChkCtlCdDs       e





    P*====================================================================
    P* CmmRcdCdDs - Get the commission record code description
    P*
    P*  CmmRcdCdDs - returns the commission record code description
    P*   co#       - accepts the company #              (required)
    P*   code      - accepts the commission record code (required)
    P*   CSrrn     - returns the RRN of WTCSP
    P*====================================================================
    P*
     PCmmRcdCdDs       b                   export
    P*
     DCmmRcdCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D CSrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   CSDESC
     C     tableKy       chain     pcs01                              60
     C                   eval      desc = CSDESC
     C*
     C                   if        %parms >= 3
     C                   eval      CSrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PCmmRcdCdDs       e





    P*====================================================================
    P* CountyCdDs - Get the county code description
    P*
    P*  CountyCdDs - returns the county code description
    P*   co#       - accepts the company #   (required)
    P*   code      - accepts the county code (required)
    P*   CNrrn     - returns the RRN of WTCNP
    P*====================================================================
    P*
     PCountyCdDs       b                   export
    P*
     DCountyCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D CNrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   CNDESC
     C     tableKy       chain     pcn01                              60
     C                   eval      desc = CNDESC
     C*
     C                   if        %parms >= 3
     C                   eval      CNrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PCountyCdDs       e





    P*====================================================================
    P* IndDisCdDs - Get the indemnity disability type description
    P*
    P*  IndDisCdDs - returns the indemnity disablity type description
    P*   co#       - accepts the company #                (required)
    P*   code      - accepts the indemnity disablity code (required)
    P*   IDrrn     - returns the RRN of WTIDP
    P*====================================================================
    P*
     PIndDisCdDs       b                   export
    P*
     DIndDisCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D IDrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   IDDESC
     C     tableKy       chain     pid01                              60
     C                   eval      desc = IDDESC
     C*
     C                   if        %parms >= 3
     C                   eval      IDrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PIndDisCdDs       e





    P*====================================================================
    P* IndSusCdDs - Get the indemnity suspension reason description
    P*
    P*  IndSusCdDs - returns the indemnity suspension reason description
    P*   co#       - accepts the company #                  (required)
    P*   code      - accepts the indemnity susp reason code (required)
    P*   IDrrn     - returns the RRN of WTIDP
    P*====================================================================
    P*
     PIndSusCdDs       b                   export
    P*
     DIndSusCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D ISrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   ISDESC
     C     tableKy       chain     pis01                              60
     C                   eval      desc = ISDESC
     C*
     C                   if        %parms >= 3
     C                   eval      ISrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PIndSusCdDs       e





    P*====================================================================
    P* LatePmtDs - Get the late payment reason description
    P*
    P*  LatePmtDs - returns the late payment reason description
    P*   co#      - accepts the company #         (required)
    P*   code     - accepts the late payment code (required)
    P*   LRrrn    - returns the RRN of WTLRP
    P*====================================================================
    P*
     PLatePmtDs        b                   export
    P*
     DLatePmtDs        PI            30
     D co#                            3  0 const
     D code                          10    const
     D LRrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   LRDESC
     C     tableKy       chain     plr01                              60
     C                   eval      desc = LRDESC
     C*
     C                   if        %parms >= 3
     C                   eval      LRrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PLatePmtDs        e





    P*====================================================================
    P* LossEduDs - Get the loss control education/training description
    P*
    P*  LossEduDs - returns the loss control description
    P*   co#      - accepts the company #         (required)
    P*   code     - accepts the loss control code (required)
    P*   TRrrn    - returns the RRN of WTTRP
    P*====================================================================
    P*
     PLossEduDs        b                   export
    P*
     DLossEduDs        PI            30
     D co#                            3  0 const
     D code                          10    const
     D TRrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   TRDESC
     C     tableKy       chain     ptr01                              60
     C                   eval      desc = TRDESC
     C*
     C                   if        %parms >= 3
     C                   eval      TRrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PLossEduDs        e





    P*====================================================================
    P* LossHygDs - Get the loss control hygiene description
    P*
    P*  LossHygDs - returns the loss control description
    P*   co#      - accepts the company #         (required)
    P*   code     - accepts the loss control code (required)
    P*   HYrrn    - returns the RRN of WTHYP
    P*====================================================================
    P*
     PLossHygDs        b                   export
    P*
     DLossHygDs        PI            30
     D co#                            3  0 const
     D code                          10    const
     D HYrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   HYDESC
     C     tableKy       chain     phy01                              60
     C                   eval      desc = HYDESC
     C*
     C                   if        %parms >= 3
     C                   eval      HYrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PLossHygDs        e





    P*====================================================================
    P* LossVstDs - Get the loss control visit description
    P*
    P*  LossVstDs - returns the loss control description
    P*   co#      - accepts the company #         (required)
    P*   code     - accepts the loss control code (required)
    P*   VSrrn    - returns the RRN of WTVSP
    P*====================================================================
    P*
     PLossVstDs        b                   export
    P*
     DLossVstDs        PI            30
     D co#                            3  0 const
     D code                          10    const
     D VSrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   VSDESC
     C     tableKy       chain     pvs01                              60
     C                   eval      desc = VSDESC
     C*
     C                   if        %parms >= 3
     C                   eval      VSrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PLossVstDs        e





    P*====================================================================
    P* NatureCdDs - Get the nature code description
    P*
    P*  NatureCdDs - returns the nature code description
    P*   co#       - accepts the company #    (required)
    P*   code      - accepts the nauture code (required)
    P*   NArrn     - returns the RRN of WTNAP
    P*====================================================================
    P*
     PNatureCdDs       b                   export
    P*
     DNatureCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D NArrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   NADESC
     C     tableKy       chain     pna01                              60
     C                   eval      desc = NADESC
     C*
     C                   if        %parms >= 3
     C                   eval      NArrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PNatureCdDs       e





    P*====================================================================
    P* NCCICdDs - Get the NCCI code description
    P*
    P*  NCCICdDs - returns the agent type description
    P*   co#     - accepts the company # (required)
    P*   code    - accepts the NCCI code (required)
    P*   NCrrn   - returns the RRN of WTNCP
    P*====================================================================
    P*
     PNCCICdDs         b                   export
    P*
     DNCCICdDs         PI            30
     D co#                            3  0 const
     D code                          10    const
     D NCrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   NCDESC
     C     tableKy       chain     pnc01                              60
     C                   eval      desc = NCDESC
     C*
     C                   if        %parms >= 3
     C                   eval      NCrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PNCCICdDs         e





    P*====================================================================
    P* PartCdDs - Get the part code description
    P*
    P*  PartCdDs - returns the part code description
    P*   co#     - accepts the company # (required)
    P*   code    - accepts the part code (required)
    P*   PArrn   - returns the RRN of WTPAP
    P*====================================================================
    P*
     PPartCdDs         b                   export
    P*
     DPartCdDs         PI            30
     D co#                            3  0 const
     D code                          10    const
     D PArrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   PADESC
     C     tableKy       chain     ppa01                              60
     C                   eval      desc = PADESC
     C*
     C                   if        %parms >= 3
     C                   eval      PArrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PPartCdDs         e





    P*====================================================================
    P* PaymntCdDs - Get the payment code description
    P*
    P*  AgtTypeDes - returns the payment code description
    P*   co#     - accepts the company #    (required)
    P*   code    - accepts the payment code (required)
    P*   PCrrn   - returns the RRN of WTPCP
    P*====================================================================
    P*
     PPaymntCdDs       b                   export
    P*
     DPaymntCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D PCrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   PCDESC
     C     tableKy       chain     ppc01                              60
     C                   eval      desc = PCDESC
     C*
     C                   if        %parms >= 3
     C                   eval      PCrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PPaymntCdDs       e





    P*====================================================================
    P* RemarkCdDs - Get the remark code description
    P*
    P*  RemarkCdDs - returns the remark code description
    P*   co#       - accepts the company #   (required)
    P*   code      - accepts the remark code (required)
    P*   RCrrn     - returns the RRN of WTRCP
    P*====================================================================
    P*
     PRemarkCdDs       b                   export
    P*
     DRemarkCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D RCrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   RCDESC
     C     tableKy       chain     prc01                              60
     C                   eval      desc = RCDESC
     C*
     C                   if        %parms >= 3
     C                   eval      RCrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PRemarkCdDs       e





    P*====================================================================
    P* RfdStsCdDs - Get the refund status code description
    P*
    P*  RfdStsCdDs - returns the agent type description
    P*   co#       - accepts the company #          (required)
    P*   code      - accepts the refund status code (required)
    P*   RSrrn     - returns the RRN of WTATP
    P*====================================================================
    P*
     PRfdStsCdDs       b                   export
    P*
     DRfdStsCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D RSrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   RSDESC
     C     tableKy       chain     prs01                              60
     C                   eval      desc = RSDESC
     C*
     C                   if        %parms >= 3
     C                   eval      RSrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PRfdStsCdDs       e





    P*====================================================================
    P* ResTypCdDs - Get the reserve type code description
    P*
    P*  ResTypCdDs - returns the reserve type code description
    P*   co#       - accepts the company #         (required)
    P*   code      - accepts the reserve type code (required)
    P*   RTrrn     - returns the RRN of WTRTP
    P*====================================================================
    P*
     PResTypCdDs       b                   export
    P*
     DResTypCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D RTrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   RTDESC
     C     tableKy       chain     prt01                              60
     C                   eval      desc = RTDESC
     C*
     C                   if        %parms >= 3
     C                   eval      RTrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PResTypCdDs       e

    P*====================================================================
    P* GetRsvType - Get the reserve type
    P*
    P*  GetRsvType - returns the reserve type
    P*   co#       - accepts the company #         (required)
    P*   code      - accepts the reserve type code (required)
    P*   Type      - returns the Type
    P*====================================================================
    P*
     PGetRsvType       b                   export
    P*
     DGetRsvType       PI             1
     D co#                            3  0 const
     D code                          10    const
     D RTrrn                          9  0       options(*nopass)

     D type            s              1

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   RTTYPE
     C     tableKy       chain     prt01                              60
     C                   eval      type = RTTYPE
     C*
     C                   if        %parms >= 3
     C                   eval      RTrrn = TB_rrn
     C                   endif
     C*
     C                   return    type
     C*
     PGetRsvType       e




    P*====================================================================
    P* SpcltyCdDs - Get the specialty code description
    P*
    P*  SpcltyCdDs - returns the specialty code description
    P*   co#       - accepts the company #           (required)
    P*   code      - accepts the specialty type code (required)
    P*   SPrrn     - returns the RRN of WTSPP
    P*====================================================================
    P*
     PSpcltyCdDs       b                   export
    P*
     DSpcltyCdDs       PI            30
     D co#                            3  0 const
     D code                          10    const
     D SPrrn                          9  0       options(*nopass)

     D desc            s             30

     C                   eval      company   = co#
     C                   eval      tableCode = code
     C*
     C                   clear                   SPDESC
     C     tableKy       chain     psp01                              60
     C                   eval      desc = SPDESC
     C*
     C                   if        %parms >= 3
     C                   eval      SPrrn = TB_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PSpcltyCdDs       e


    P*====================================================================
    P* ValidGenCode - Validate generic table code
    P*  Returns  - '1' is valid code, '0' if invalid code
    P*   co#       - accepts the company # (required)
    P*   tbid      - accepts the table id (required)
    P*   key/code  - key or code value (required)
    P*   prefix    - possible table id prefix (optional)
    P*====================================================================
    P*
     PValidGenCode     b                   export
    P*
     DValidGenCode     pi              n
     D co#                            3  0 const
     D tbid                           4    const
     D***  Begin Chg  ***  07/10/19  *******************************
     D*key                           10    const
     D key                           10    const varying
     D***  End   Chg  ***  07/10/19  *******************************
     D***  Begin Add  ***  09/04/19  *******************************
     D inPrefix                       4    const varying options(*nopass)

     D strTbId         s              4    varying inz
     D strPrefix       s              4    varying inz
     D***  End   Add  ***  09/04/19  *******************************

      /free
       //***  Begin Chg  ***  09/04/19  *******************************
       //**********Chain (co#:tbid:key) pgt00;
       If %parms >= 4 and inPrefix <> *blank;
          strPrefix = inPrefix;
       Endif;
       strTbId = GetGenTableId(co#:tbid:strPrefix);
       Chain (co#:strTbId:key) pgt00;
       //***  End   Chg  ***  09/04/19  *******************************
       If %found(wtgtl);
         Return *On;
       Else;
         Return *Off;
       Endif;
      /end-free
     PValidGenCode     e


       //***  Begin Add  ***  09/04/19  *******************************
    P*====================================================================
    P* GetGenTableId - Get generic table id
    P*  Returns  - 4A id
    P*   co#       - accepts the company # (required)
    P*   tbid      - accepts the table id (required)
    P*   prefix    - possible table id prefix (optional)
    P*====================================================================
     PGetGenTableId    b                   export
     DGetGenTableId    pi             4
     D inCo#                          3  0 const
     D inTbId                         4    const varying
     D inPrefix                       4    const varying options(*nopass)

     D strTbId         s              4    inz
     D strPrefix       s              4    varying inz
      /Free
       If %parms >= 3 and inPrefix <> *blank;
          strPrefix = inPrefix;
       Endif;
       strTbId = strPrefix + inTbId;
       Chain (inCo#:strTbId) pgi00;
       If %found;
          Return strTbId;
       Endif;
       strTbId = inTbId;
       Chain (inCo#:strTbId) pgi00;
       If %found;
          Return strTbId;
       Endif;
       strTbId = RightJstfy(inTbId:4);
       Chain (inCo#:strTbId) pgi00;
       If %found;
          Return strTbId;
       Endif;
       Return inTbId;
      /End-free
     PGetGenTableId    e
       //***  End   Add  ***  09/04/19  *******************************

    P*====================================================================
    P* GetGenCodeDs - Get generic code description
    P*  Returns  - 40A descriptio
    P*   co#       - accepts the company # (required)
    P*   tbid      - accepts the table id (required)
    P*   key/code  - key or code value (required)
    P*   prefix    - possible table id prefix (optional)
    P*====================================================================
     PGetGenCodeDs     b                   export
     DGetGenCodeDs     pi            40
     D co#                            3  0 const
     D tbid                           4    const
     D***  Begin Chg  ***  07/10/19  *******************************
     D*key                           10    const
     D key                           10    const varying
     D***  End   Chg  ***  07/10/19  *******************************
     D***  Begin Add  ***  09/04/19  *******************************
     D inPrefix                       4    const varying options(*nopass)

     D strTbId         s              4    varying inz
     D strPrefix       s              4    varying inz
     D***  End   Add  ***  09/04/19  *******************************

      /free
       //***  Begin Chg  ***  09/04/19  *******************************
       //**********Chain (co#:tbid:key) pgt00;
       If %parms >= 4 and inPrefix <> *blank;
          strPrefix = inPrefix;
       Endif;
       strTbId = GetGenTableId(co#:tbid:strPrefix);
       Chain (co#:strTbId:key) pgt00;
       //***  End   Chg  ***  09/04/19  *******************************
       If %found(wtgtl);
         Return gtdesc;
       Else;
         Return *blanks;
       Endif;
      /end-free
     PGetGenCodeDs     e

      //====================================================
      // Retrieves payroll class code description
    P* inCo#        - accepts the company # (required)
    P* inEfDt       - accepts the effective date (required)
    P* inState      - accepts the state (required)
    P* inRtCd       - accepts the rate set code (required)
    P* inClas       - accepts the class code (required)
    P* inExt        - accepts the extension (required)
     P* outHazGrp    - Returns Hazard Group (Optional)
     P* outContClass - Returns Contractor Class (Optional)
      //====================================================
     pGetPayrollDesc   b                   export
     dGetPayrollDesc   pi            30
     d inCo#                          3  0 const
     d inEfDt                         7s 0 const
     d inState                        2    const
     d inRtCd                         3    const
     d inClas                         5s 0 const
     d inExt                          5    const
     d outHazGrp                      5    options(*nopass)
     d outContClass                   1    options(*nopass)
     d**** Beg Chg ***** 06/17/24 ******************************************
     d***outRate                        7s 2 options(*nopass)
     d outRate                        8s 3 options(*nopass)
     d**** End Chg ***** 06/17/24 ******************************************

     d outDesc         s             30
     d hazGroup        s              5
     d contClass       s              1
     d rate            s              7  2

      /Free
       Exec SQL
        Select PRdesc, PRhzGr, PRccFl, PRrt
        Into :outDesc, :hazGroup, :contClass, :rate
        From WMPRP
        Where PRco#  = :inCo#
        And PRrtcd  = :inRtCd
        And PRclas  = :inClas
        And PRefdt <= :inEfDt
        And PRstat  = 'A'
        Order by PRefdt Desc
        Limit 1;

       If %Parms() > 6;
          outHazGrp    = hazGroup;
       EndIf;

       If %Parms() > 7;
          outContClass = contClass;
       EndIf;

       If %Parms() > 8;
          outRate      = rate;
       EndIf;

       If inExt <> *Blanks;
       //***  Begin Chg  ***  11/04/20  **************
       // Exec Sql
       //  Select PXdesc
       //  Into :outDesc
       //  From WMPXP
       //  Where PXco#  = :inCo#
       //   And PXclas  = :inClas
       //   And PXst    = :inState
       //   And PXefdt <= :inEfDt
       //   And PXstat  = 'A'
       //  Order by PXefdt
       //  Limit 1;
          Exec Sql
           Select PXdesc
           Into :outDesc
           From WMPXP
           Where PXco#  = :inCo#
            And PXclas  = :inClas
            And PXst    = :inState
            And PXext   = :inExt
            And PXefDt <= :inEfDt
            And PXstat  = 'A'
           Order by PXefDt desc
           Limit 1;
       //***  End   Chg  ***  11/04/20  **************
       EndIf;

       Return outDesc;

      /End-Free
     pGetPayrollDesc   e
