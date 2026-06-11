     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('WTBNDDIR')
     H*====================================================================
     H/TITLE WXMLDATA - Generate work file data for WCMS Scriptura requests.
     H*====================================================================
     H*  Description:
     H*
     H*  Build work file data to be used for Scriptura Document jobs on
     H*  WCMS processes.  This program will accept the XML document name
     H*  as an entry parameter to chain to the XMLPREP file to retrieve
     H*  the job specific information for generating data.
     H*
     H*  Called from:  STLETTER
     H*
     H*  Created:  04/24/15  C:65  Project: SY251
     H*====================================================================
     H*  Revisions:
     A*
     A* 09/26/25 C:65  Support: 119568 (Prescient)
     A*                Add ASO flag set at group for PEO groups.
     A*
     A* 07/03/25 C:300 Support: 117034 (UBIC)
     A*                Add Insured email for Audit Request Form
     H*
     H* 05/08/25 C:333 Project: 153WBD
     H*                Note of Cancel Format-Contact Info Updt
     H*
     H* 04/28/25 C:291 Support: 118949 (CoveRisk)
     H*                Don't use state reporting division name address for
     H*                all divisions if called from Premium Notice.
     H*                This fix should be installed with PHW 118418
     H*
     H* 02/04/25 C:291 Support: 118418
     H*                Generate work file data for XMLTEMP records associated
     H*                with each XMLPREP.
     H*                This fix should be installed with PHW 118949
     H*
     H* 05/17/24 C:62  Project: WC2670(115597(099))
     H*                Don't use state reporting division name address for
     H*                all divisions if called from billing.
     H*
     H* 02/12/24 C:65  Project: WC2670
     H*                Call SRADDR with 'F' for name and 'M' for address
     H*                flags only for compliance output.  Call with blank
     H*                values for everything else to mimic how output was
     H*                generated before Scriptura like bills.
     H*
     H* 01/16/24 C:340 Project: 153WAD
     H*                Populate Office Email Address.
     H*
     H* 05/10/23 C:62  Project: 148WEF
     H*                Populate the new type of business desc field
     H*
     H* 12/12/22 C:79  Support: 129/105507
     H*                Fix issues with cert holder amounts
     H*
     H* 02/22/22 C:62  Project: 150WFN
     H*                Use new GetNameAddress.
     H*
     H*  04/21/21 C:65  Project: Phw#95592
     H*                 Remove trim/insert of single blank from employer
     H*                 name fields.
     H*
     H*  06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*
     H*  11/12/18 C:65  Project: SY371/Phw#71566 (Amwins)
     H*                 Change the PEO Type field, FNPEOF, to have a value
     H*                 based from the state for combined groups.
     H*
     H*
     H* 06/11/18 C:62  Project: 102WHM
     H*                Add pre-injury compensation rate.
     H*
     H* 05/01/18 C:65  Project: 150WAD
     H*                Change the output control flag to SRADDR for
     H*                CA policies which will use acronym to allow it
     H*                to be appended to client name instead of PEO.
     H*
     H*  04/03/18 C:53  Support: 64520
     H*                 Correct DOCMACTM population.
     H*
     H*  03/26/18 C:53  Project: 146WHH
     H*                 Process additional fields for claim letters.
     H*                 Correct index error on
     H*
     H*  03/05/18 C:65  Project: SY371
     H*                 Ensure SRADDR passes 'b' for blank output control
     H*                 mode as well as DBA flag for NJ policies.
     H*
     H*  01/15/18 C:65  Project: 150WCJ
     H*                 Pull PEO name from additional named insured if
     H*                 indicated on policy.
     H*
     H*  11/27/17 C:65  Project: 102WHK
     H*                 Populate pre-injury average weekly wage.
     H*
     H*  10/31/17 C:65  Project: Phw#50723
     H*                 Read agent history until active record is found.
     H*
     H*  10/12/17 C:65  Project: SY371
     H*                 Populate formatted name/address fields for both
     H*                 division and policy levels.
     H*
     H*  10/11/17 C:65  Project: 150WAP
     H*                 Recognize Combined type PEO groups.
     H*
     H*  03/08/17 C:65  Project: SY371
     H*                 Add group NAIC code.  Add policy limits.
     H*
     H*  03/01/17 C:65  Project: SY371
     H*                 Populate PEO data.  Move the NJ tax ID to after the
     H*                 chain on rating state and the PEO section out of the
     H*                 NJ if condition.
     H*
     H*  01/10/17 C:65  Project: SY371
     H*                 Pull last agent history record instead of current.
     H*                 Populate commerical/self-insured group flag,
     H*                 employer mailing/physical address flag, and cancel
     H*                 reason code/description fields.
     H*
     H*  11/22/16 C:65  Project: SY371
     H*                 Populate claim received date.
     H*
     H*  11/16/16 C:65  Project: SY371
     H*                 Check XPData on individual records.
     H*
     H*  10/20/16 C:65  Project: SY371
     H*                 Populate the rating state of the policy.
     H*
     H*  10/13/16 C:65  Project: SY371
     H*                 Populate extra fields for policy header schema.
     H*
     H*  10/04/16 C:65  Project: SY371
     H*                 Prevent looping issue for mass requests.
     H*
     H*  09/27/16 C:65  Project: SY371
     H*                 Add agent heading.
     H*
     H*  08/04/16 C:65  Project: SY371
     H*                 Key the work file data with the template XML filename.
     H*
     H*  05/31/16 C:65  Project: SY371
     H*                 Add processing to loop over records linked to a
     H*                 "master" record to generate work file data.
     H*
     H*  04/29/16 C:65  Project: SY251
     H*                 Change subrogration flag field to hold description.
     H*                 Add fields to hold place of accident.
     H*
     H*  04/20/16 C:65  Project: SY251
     H*                 Change lost time flag field to hold description.
     H*
     H*  04/08/16 C:65  Project: SY371
     H*                 Populate attorney names from payee demo file if
     H*                 name is not populated in the legal master file.
     H*                 Pull the judge name from generic table file for
     H*                 trial info.
     H*
     H*  01/07/16 C:65  Project: SY251
     H*                 Add office fields for policy and claim offices.
     H*                 Add part/cause/nature fields for claims.
     H*                 Add new workfile for claim trial information.
     H*
     H*  01/05/16 C:65  Project: SY251
     H*                 Add monitor to billing address zip code in case
     H*                 record has no address/zip code on it.
     H*
     H*  12/18/15 C:65  Project: SY251
     H*                 Add categor 2 heading/value fields for claims.
     H*
     H*  12/15/15 C:65  Project: SY251
     H*                 Increment key field for contact workfile for java.
     H*
     H*  08/06/15 C:65  Project: SY251
     H*                 Add phone/fax fields for each staff/user.
     H*
     H*  07/31/15 C:65  Project: SY251
     H*                 Add new fields to program: company mailing address,
     H*                 phone numbers, and claim appeal bureau number.
     H*
     H*====================================================================
     FXMLPREL1  IF   E           K DISK
     F*** Beg Add *****  02/04/25  **************************************
     FXMLTEML   IF   E           K DISK
     F*** End Add *****  02/04/25  **************************************
     FWMEML     if   e           k disk    rename(pem01:pem00)
     FWMA3L     if   e           k disk    rename(pa301:pa300)
     FWDELL     if   e           k disk    rename(pel01:pel00)
     FWMRPL     if   e           k disk    rename(prp01:prp00)
     FWMAHL     if   e           k disk    rename(pah01:pah00)
     FWMAPL     if   e           k disk    rename(pap01:pap00)
     FWMAAL1    if   e           k disk    rename(paa01:paa01)
     FWMAGL     if   e           k disk    rename(pag01:pag00)
     FWMECL     if   e           k disk    rename(pec01:pec00)
     FWMCML     if   e           k disk    rename(pcm01:pcm00)
     FWMCDL     if   e           k disk    rename(pcd01:pcd00)
     FWMCHL5    if   e           k disk    rename(pch01:pch05)
     FWMPRL1    if   e           k disk    rename(ppr01:ppr01)
     FWMLLL     if   e           k disk    rename(pll01:pll00)
     FWMFNL     if   e           k disk    rename(pfn01:pfn00)
     FSMCOL     if   e           k disk    rename(pco01:pco00)
     FWTCNL1    if   e           k disk    rename(pcn01:pcn01)
     FWMREL     if   e           k disk    rename(pre01:pre00)
     FWMCPL     if   e           k disk    rename(pcp01:pcp00)
     FWTGTL     if   e           k disk    rename(pgt01:pgt00)
     FWMUPL1    if   e           k disk    rename(pup01:pup01)
     FWTOCL     if   e           k disk    rename(poc01:poc00)
     FWMDPL     if   e           k disk    rename(pdp01:pdp00)
     FWMTPL1    if   e           k disk    rename(ptp01:ptp01)
     F*** begin add ***  12/18/15  **************************************
     FWMCXL     if   e           k disk    rename(pcx01:pcx00)
     F*** end add *****  12/18/15  **************************************
     F*** begin add ***  01/07/16  **************************************
     FWMOFL     if   e           k disk    rename(pof01:pof00)
     FSTABL     if   e           k Disk    Include(pst01)
     FWMTIL     if   e           k Disk    ReName(pti01:pti00)
     FSXDOP2    o    e             disk    rename(pdo012:pdo0p2)
     F                                     prefix(D2:2)
     F*** end add *****  01/07/16  **************************************
     F*** begin add ***  04/08/16  **************************************
     FWMPDL     if   e           k Disk    ReName(ppd01:ppd00)
     F*** end add *****  04/08/16  **************************************
     FWTABL     if   e           k disk    ignore(pcn01)
     FSXDOP     o    e             disk    rename(pdo01:pdo0p)
     FSXDOP1    o    e             disk    rename(pdo011:pdo0p1)
     F                                     prefix(DC:2)
     F*** begin add ***  04/08/16  **************************************
     FWMEXL     if   e           k Disk    ReName(pex01:pex00)
     F*** end add *****  04/08/16  **************************************
     D*** begin add ***  10/13/16  **************************************
     FWMRSL     if   e           k Disk    ReName(prs01:prs00)
     FWDF2L     if   e           k Disk    ReName(pf201:pf200)
     D*** end add *****  10/13/16  **************************************
     D*** begin add ***  01/10/17  **************************************
     FWDCXL1    IF   E           K DISK
     FWTCRL     IF   E           K DISK    rename(pcr01:pcr00)
     D*** end add *****  01/10/17  **************************************
     D*** begin add ***  03/01/17  **************************************
     FWDRRL     IF   E           K DISK    rename(prr01:prr00)
     D*** end add *****  03/01/17  **************************************
     D*** begin add ***  03/01/17  **************************************
     FWTLML1    IF   E           K DISK
     F                                     RENAME(PLM01:PLM01)
     D*** end add *****  03/01/17  **************************************
     F*** begin add ***  11/27/17  **************************************
     FWMAWL1    IF   E           K DISK    rename(paw01:paw01)
     F*** end add *****  11/27/17  **************************************
     F*** begin add ***  01/15/18  **************************************
     FWMANL1    IF   E           K DISK    rename(pan01:pan01)
     F*** end add *****  01/15/18  **************************************
     F*** begin add ***  03/26/18  **************************************
     FSMUPL1    if   e           k disk    rename(pup01:pup02)
     F                                     prefix(SS)
     FWdpal     if   e           k disk    rename(ppa01:ppa00)
     F*** END add ***  03/26/18  ****************************************
     D*
     D PSDS          ESDS                  extname(SZQ1P)
     D*
     D xmlFile         s                   like(XPXML)
     d*** begin add ***  05/31/16  ****************************************
     d xmlLink         s             60    inz(' ')
     d xmlFile2        s             60    inz(' ')
     d xxProcess       s              1    inz(' ')
     d*** end add *****  05/31/16  ****************************************
     D*
     D keyCo#          s                   like(CMCO#)
     D keyFnd          s                   like(CMFND)
     D keyEmp#         s                   like(CMEMP#)
     D keyDiv          s                   like(CMDIV)
     D keyFyr          s                   like(CMFYR)
     D keyCase         s                   like(CMCASE)
     D*
     D xxKey           s                   like(CNKEY)
     D xxTbid          s                   like(GTTBID)
     D xxAgt#          s                   like(AHAGT#)
     D xxProd          s                   like(AHPROD)
     D xxFein          s                   like(AGFEIN)
     D xxMod           s                   like(AGMOD)
     D xxResType       s                   like(RTDESC)
     D xxEffDate       s                   like(ELPFDT)
     D*** begin add ***  01/15/18  **************************************
     D xxSeq#          s              5  0
     D*** end add *****  01/15/18  **************************************
     D curDate         s                   like(ELPFDT)
     D outDate         s              7
     D*** begin add ***  03/08/17  **************************************
     D xxDate          s                   like(ELPFDT)
     D*** end add *****  03/08/17  **************************************
     D*** begin add ***  12/15/15  **************************************
     D xxCount         s              9  0
     D*** end add *****  12/15/15  **************************************
     D*** begin add ***  04/08/16  **************************************
     D kkFein          s                   like(PDFEIN)
     D kkMod           s                   like(PDMOD)
     D*** end add *****  04/08/16  **************************************
     D*** begin add ***  03/05/18  *********************************
     D xxNJSt          s                   like(RSJRST)
     D*** end add *****  03/05/18  *********************************
     D*** begin add ***  10/13/16  **************************************
     d clientID        s              3    inz('000')
     D
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     D*** end add *****  10/13/16  **************************************
     D*** Begin Add *** 05/17/24 ****************************************
     d isCalledFromBilling...
     d                 s              3    Inz(*Off)
     D*** End   Add *** 05/17/24 ****************************************
     D*** Beg Add ***** 02/04/25 **************************************
     d strXmlT         s                   Like(XPXMLT)
     D*** End Add ***** 02/04/25 **************************************
     D*** begin add ***  10/12/17  **********************************
     D stRptDiv        s                   like(CMDIV)
     d ##co#           s              3
     d ##fnd           s              3
     d ##emp#          s              9
     d ##div           s              5
     d ##odr           s              1
     d ##nodr          s              1
     d ##aodr          s              1
     d ##emn1          s             50
     d ##emn2          s             50
     d ##emn3          s             50
     d ##emn4          s             50
     d ##emn5          s             50
     d ##emn6          s             50
     d ##emn7          s             50
     d ##adrl          s              3  0
     d ##naml          s              3  0
     d ##fedl          s              3  0
     d ##peo1          s             57
     d ##peo2          s             57
     d ##pgfl          s              1
     d ##conl          s              3  0
     d ##peol          s              3  0
     d ##dbaf          s              1
     d ##al2           s              3  0
     d ##nl2           s              3  0
     d ##fl2           s              3  0
     d ##pl2           s              3  0
     d ##cl2           s              3  0
     d ##peoext        s             10
     d ##name1         s            120
     d ##name2         s            120
     d ##name3         s            120
     d ##name4         s            120
     d*** Begin Add *** 02/22/22 *******************************************
     d ##fein          s              9  0
     d*** End   Add *** 02/22/22 *******************************************

     d SrAddr          pr                  extpgm('SRADDR')
     d ##co#                          3    const
     d ##fnd                          3    const
     d ##emp#                         9    const
     d ##div                          5    const
     d ##odr                          1
     d ##nodr                         1
     d ##aodr                         1
     d ##emn1                        50
     d ##emn2                        50
     d ##emn3                        50
     d ##emn4                        50
     d ##emn5                        50
     d ##emn6                        50
     d ##emn7                        50
     d ##adrl                         3  0
     d ##naml                         3  0
     d ##fedl                         3  0
     d ##peo1                        57
     d ##peo2                        57
     d ##pgfl                         1
     d ##conl                         3  0
     d ##amn1                        60
     d ##amn2                        60
     d ##amn3                        60
     d ##amn4                        60
     d ##amn5                        60
     d ##amn6                        60
     d ##amn7                        60
     d ##amn8                        60
     d ##amn9                        60
     d ##peol                         3  0
     d ##dbaf                         1
     d ##al2                          3  0
     d ##nl2                          3  0
     d ##fl2                          3  0
     d ##pl2                          3  0
     d ##cl2                          3  0
     d ##peoext                      10
     d ##name1                      120
     d ##name2                      120
     d ##name3                      120
     d ##name4                      120

     D                 ds
     D  amn                    1    540    dim(9)
     D  ##amn1               001    060
     D  ##amn2               061    120
     D  ##amn3               121    180
     D  ##amn4               181    240
     D  ##amn5               241    300
     D  ##amn6               301    360
     D  ##amn7               361    420
     D  ##amn8               421    480
     D  ##amn9               481    540
     D*** end add *****  10/12/17  **********************************
     D*** begin add ***  03/26/18  ********************************         *****
     D                 ds
     D  SSUPAlt#               1     10  0
     D  ##UPAltA               1      3  0
     D  ##UPAltP               4     10  0
     D*** end add *****  03/26/18  ********************************         *****
     D*
     D/copy sPrDate
     D/copy sPrEnv
     D/copy WprEmplr
     D***  Begin Add  ***  06/09/20  ****************************
     D/copy sPrMask
     D***  End   Add  ***  06/09/20  ****************************
     D*** Begin Add *** 05/17/24 **************************************
     D/copy sPrJobInfo
     D*** End   Add *** 05/17/24 **************************************
     D*
     d SrEffD          pr                  extpgm('SREFFD')
     d   ##CO#                        3    const
     d   ##FND                        3    const
     d   ##FYR                        3    const
     d   ##EMP#                       9    const
     d   ##DIV                        5    const
     d   ##Date                       7
     d   ##Type                       1    const
     d   ##Clas                       5    const
     C*
     C* Key list for Employer Master File
     C*
     C     keyWMEML      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C*
     C* Key list for Employer Detail File
     C*
     C     keyWDELL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    keyFyr
     C*
     C* Key list for Agent History File
     C*
     C     keyWMAHL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    keyFyr
     C*** begin del ***  01/10/17  *********************************************
     C*********          kfld                    curDate
     C*** end del *****  01/10/17  *********************************************
     C*
     C     key1WMAHL     klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    keyFyr
     C*
     C* Key list for Agent Assignment File
     C*
     C     keyWMAAL1     klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    xxAgt#
     C*
     C* Key list for Agent Master File
     C*
     C     keyWMAGL      klist
     C                   kfld                    keyCo#
     C                   kfld                    xxFein
     C                   kfld                    xxMod
     C*
     C* Key list for Agent Producer File
     C*
     C     keyWMAPL      klist
     C                   kfld                    keyCo#
     C                   kfld                    xxFein
     C                   kfld                    xxMod
     C                   kfld                    xxProd
     C*
     C* Key list for Employer Contacts File
     C*
     C     keyWMECL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C*
     C* Key list for Claim Master File
     C*
     C     keyWMCML      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    keyFyr
     C                   kfld                    keyCase
     C*
     C* Key list for Claim History File
     C*
     C     keyWMCHL5     Klist
     C                   Kfld                    keyCo#
     C                   Kfld                    keyFnd
     C                   Kfld                    keyFyr
     C                   Kfld                    keyCase
     C                   Kfld                    curDate
     C*
     C     key1WMCHL5    Klist
     C                   Kfld                    keyCo#
     C                   Kfld                    keyFnd
     C                   Kfld                    keyFyr
     C                   Kfld                    keyCase
     C*
     C* Key list for Physician File
     C*
     C     keyWMTPL1     Klist
     C                   Kfld                    keyCo#
     C                   Kfld                    keyFnd
     C                   Kfld                    keyFyr
     C                   Kfld                    keyCase
     C                   Kfld                    curDate
     C*
     C     key1WMTPL1    Klist
     C                   Kfld                    keyCo#
     C                   Kfld                    keyFnd
     C                   Kfld                    keyFyr
     C                   Kfld                    keyCase
     C*
     C* Key list for Payroll Master File
     C*
     C     keyWMPRL1     klist
     C                   kfld                    keyCo#
     C                   kfld                    ELRtcd
     C                   kfld                    CMClas
     C                   kfld                    xxEffDate
     C*
     C     key1WMPRL1    klist
     C                   kfld                    keyCo#
     C                   kfld                    ELRtcd
     C                   kfld                    CMClas
     C*
     C* Key list for Department Master File
     C*
     C     keyWMDPL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    CMDept
     C*
     C* Key list for Legal Master File
     C*
     C     keyWMLLL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyFyr
     C                   kfld                    keyCase
     C*
     C* Key list for Billing Address File
     C*
     C     keyWMA3L      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C*
     C* Key list for Carrier Policy # File
     C*
     C     keyWMRPL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    keyFyr
     C*
     C* Generic Table File Key List
     C*
     C     keyWTGTL      Klist
     C                   Kfld                    keyCO#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
     C*
     C* Table File Key List
     C*
     C     keyWTABL      Klist
     C                   Kfld                    keyCO#
     C                   Kfld                    xxKey
     C*** begin add ***  12/18/15  **************************************
     C*
     C* Claim/Darts Control File Key List
     C*
     C     keyWMCXL      Klist
     C                   Kfld                    keyCO#
     C                   Kfld                    keyFnd
     C*** end add *****  12/18/15  **************************************
     C*** begin add ***  12/18/15  **************************************
     C*
     C* Claim Trial File Key List
     C*
     C     keyWMTIL      Klist
     C                   Kfld                    keyCO#
     C                   Kfld                    keyFnd
     C                   Kfld                    keyFyr
     C                   Kfld                    keyCase
     C*** end add *****  12/18/15  **************************************
     C*** begin add ***  04/08/16  **************************************
     C*
     C* Payee Demographics File Key List
     C*
     C     keyWMPDL      Klist
     C                   Kfld                    kkFein
     C                   Kfld                    kkMod
     C*** end add *****  04/08/16  **************************************
     C*** begin add ***  05/31/16  ****************************************
     C     key1XML       klist
     C                   kfld                    xmlFile2
     C                   kfld                    xmlLink
     C*** end add *****  05/31/16  ****************************************
     C*
     C* Entry list to program.
     C*
     C     *entry        plist
     C                   parm                    QQXML            60
     C*
     C                   Movel(p)  QQXML         xmlFile
      /free
       //*** begin add ***  10/13/16  **************************************
       GetClientID(clientID);
       //*** end add *****  10/13/16  **************************************

       //*** Begin Add *** 05/17/24 **************************************
       // Determine if called from billing process (W7615R or W7700R)
       isCalledFromBilling = *Off;
       //*** Beg Chg ***** 04/28/25 *****************************
       //***If PgmInStack('W7615R') Or PgmInStack('W7700R');
       If PgmInStack('W7615R') Or PgmInStack('W7700R')
        or PgmInStack('W7321R');
       //*** End Chg ***** 04/28/25 *****************************
          isCalledFromBilling = *On;
       EndIf;
       //*** End   Add *** 05/17/24 **************************************

       //* Chain to XML Prep file to retrieve job information.
       Chain xmlFile xp100;
       If not %found;
          *inLR = *on;
          Return;
       Endif;

       //*** begin add ***  05/31/16  **************************************************
       xxProcess = ' ';

       If XPLink = ' ';
          xxProcess = 'Y';
       Else;
          xmlFile2 = ' ';
          xmlLink = XPLink;
          Setll key1XML xp100;
          Reade(n) key1XML xp100;
          If not %eof;
             xxProcess = 'Y';
          Endif;
       Endif;

       Dow xxProcess = 'Y';
       //*** end add *****  05/31/16  **************************************************
       //*** begin add ***  11/16/16  **************************************************
          If XPData = 'Y';
       //*** end add *****  11/16/16  **************************************************

             keyCo#  = XPCo#;
             keyFnd  = XPFnd;
             keyEmp# = XPEmp#;
             keyDiv  = XPDiv;
             keyFyr  = XPFyr;
             keyCase = XPCase;

             Chain keyWMEML pem00;
             Chain keyWDELL pel00;

             If XPCat = 'CS';
                Chain keyWMCML pcm00;
             Endif;

             If not %found;
                *inLR = *on;
                Return;
             Endif;

             //*** Beg Add ***** 02/04/25 *********************************************
             //Write SXDOP for XMLPREP record
             strXmlT = XPXMLT;
             Exsr WriteSXDOP;

             //Write SXDOP for associated XMLTEMP record
             Setll XPFile xt100;
             Reade XPFile xt100;
             Dow Not %Eof(XMLTEML);
                If XTDATA = 'Y';
                   keyCo#  = XTCo#;
                   keyFnd  = XTFnd;
                   keyEmp# = XTEmp#;
                   keyDiv  = XTDiv;
                   keyFyr  = XTFyr;
                   Clear keyCase;
                   strXmlT = XTXMLT;
                   Chain keyWMEML pem00;
                   Chain keyWDELL pel00;
                   Exsr WriteSXDOP;
                Endif;
                Reade XPFile xt100;
             Enddo;
             //*** End Add ***** 02/04/25 *********************************************

       //*** Beg Del ***** 02/04/25 *********************************************
       //*************** Following code moved to subroutine WriteSXDOP  *********
       //***curDate = Dateto7(SystemDate());

       //* Clear record format for workfile and populate key fields.
       //***Clear pdo0p;

       //*** begin chg ***  08/04/16  ******************************************
       //*****DOXml  = xmlFile;
       //***DOXml  = XPXMLT;
       //*** end chg *****  08/04/16  ******************************************
       //***DOCo#  = keyCo#;
       //***DOFnd  = keyFnd;
       //***DOEmp# = keyEmp#;
       //***DODiv  = keyDiv;
       //***DOFyr  = keyFyr;
       //***DOCase = keyCase;

       //* Populate fields associated with each area of work file.
       //***Exsr Company;
       //***Exsr Group;
       //***Exsr Employer;
       //***Exsr PolicyYear;
       //***Exsr Agent;
       //***Exsr Contacts;

       //* Populate claim data if needed.
       //***If XPCat = 'CS';
          //***Exsr Claim;
          //*** begin add ***  01/07/16  **********************************
          //***Exsr Trials;
          //*** end add *****  01/07/16  **********************************
          //*** begin add ***  03/26/18  ***********************************
          //***Exsr Payments;
          //*** end add *****  03/26/18  ***********************************
       //***Endif;

       //***DOEnviro = CheckEnv();

       //*** begin add ***  10/13/16  ******************************************
       //***DOPolHdg  = 'Carrier Policy #:';
       //***DOPPolHdg = 'Carrier Prior Policy #:';
       //***DONJTaxHd = 'N.J. Taxpayer Identification No.:';
       //***DOFileHdg = 'File #:';
       //***DOPEOTaxH = 'L/C/F N.J. Tax Id No.:';

       //***Chain (keyCo#:keyFnd:keyEmp#:keyDiv:keyFyr-1) pel00;
       //***If %found;
          //***DOPPol# = %editc(keyFnd:'X') + %editc(keyEmp#:'X') +
          //*** %editc(keyFyr-1:'X');
          //***Chain (keyCo#:keyFnd:keyEmp#:keyDiv:keyFyr-1) prp00;
          //***If %found;
             //***DOPPOL# = %trim(RPCap#);
          //***Endif;
       //***Else;
          //***DOPPol#   = 'New';
       //***Endif;
       //*** Beg Add *** 12/12/22 *********************************************
       // Get the original WDELP record for the Fyr being processed.
       //***Chain keyWDELL pel00;
       //*** End Add *** 12/12/22 *********************************************

       //***Chain (keyCo#:ELRTCD) prs00;
       //*** begin add ***  10/20/16  **********************************************
       //***If %found;
          //***DORateSt = RSJRST;
       //***Endif;
       //*** end add *****  10/20/16  **********************************************
       //***If %found and RSJRST = 'NJ';
          //***If clientID = '114';
             //***DONJTaxId = %trim(EMUE#);
          //***Else;
             //***DONJTaxId = %trim(EMSEMP);
          //***Endif;
       //***Endif;

       //*** begin add ***  03/08/17  ***************************************
       //* Set the policy limits
       //***If ELIlcd = *blank;

          //***Setgt (keyCo#:keyFnd:DoRateSt:ELPfdt) prr00;
          //***Readpe (keyCo#:keyFnd:DoRateSt) prr00;
          //***If not %eof;
             //***DOLmtAcc = 1000 * RRAcc;
             //***DOLmtDis = 1000 * RRDis;
             //***DOLmtAgg = 1000 * RRAgg;
          //***Endif;

       //***Else;

          //***If ELInl2 <> ' ' and ELI2ef <= curDate;
          //***   xxKey  = ELInl2;
          //***Else;
          //***   xxKey  = ELIlcd;
          //***Endif;

          //***Setgt (keyCo#:keyFnd:DORateSt:xxKey:ELPfdt) plm01;
          //***Readpe(n) (keyCo#:keyFnd:DORateSt:xxKey) plm01;
          //***If not %eof;
          //***   DOLmtAcc = 1000 * LMAcc;
          //***   DOLmtDis = 1000 * LMDis;
          //***   DOLmtAgg = 1000 * LMAgg;
          //***Endif;

       //***Endif;
       //*** end add *****  03/08/17  ***************************************

       //*** begin add ***  01/10/17  ***************************************
       //* Populate cancellation reason code/description if cancelled.
       //***If DOEMCxdt <> 0 and DORateSt <> '  ';
       //***   Chain(n) (EMCo#:EMFnd:EMEmp#:EMDiv:DOEmCxdt) pcx01;
       //***   If %found;
       //***      DOCxRsn = CXRsn;
       //***      xxKey = CXRSN;
       //***      Setgt (EMCo#:DORateSt:xxKey:curDate) pcr00;
       //***      Readpe(n) (EMCo#:DoRateSt:xxKey) pcr00;
       //***      Dow not %eof and CRStat = 'I';
       //***         Readpe(n) (EMCo#:DoRateSt:xxKey) pcr00;
       //***      Enddo;

       //***      If not %eof;
       //***         DOCxRsnD = CRDesc;
       //***      Endif;

       //***   Endif;
       //***Endif;
       //*** end add *****  01/10/17  ***************************************

       //*** begin add ***  03/01/17  ******************************
       //***If FNPEOF <> ' ';

       //***   Setgt (keyCo#:keyFnd:DoRateSt:ELPfdt) prr00;
       //***   Readpe (keyCo#:keyFnd:DoRateSt) prr00;
       //***   If not %eof;
       //***      DOPEOPGFL = RRPGFL;
       //***      DOPEOAcro = RRPEOA;
       //***   //*** begin add ***  11/12/18  *******************************
       //***      If FNPEOF = 'C' and RRPEOF <> '' ;
       //***         DOFnPEOF = RRPEOF;
       //***      Endif;
          //*** end add *****  11/12/18  *******************************
       //***   Endif;
       //*** end add *****  03/01/17  ******************************

       //*** begin chg ***  10/11/17  ******************************
       //******If keyEmp# <> 1 and (FNPEOF = 'M' or FNPEOF = 'Y');
       //***   If keyEmp# <> 1 and (FNPEOF = 'M' or FNPEOF = 'Y' or FNPEOF = 'C');
       //*** end chg *****  10/11/17  ******************************
          //*** begin add ***  01/15/18  ******************************
       //***      xxSeq# = EMPEONMIN;
          //*** end add *****  01/15/18  ******************************
       //***      Chain (keyCo#:keyFnd:1:0) pem00;
       //***      If %found;
             //*** begin chg ***  03/01/17  ******************************
             //***   If EMFdnm <> ' ';
             //***      DOPEOName = %trim(EMFdnm) + ' ' + %trim(EMFdn2);
             //***   Else;
             //***      DOPEOName = %trim(EMName) + ' ' + %trim(EMNam2);
             //***   Endif;
             //*** begin chg ***  04/21/21  *********************************
             //*****   DOPEOName = %trim(EMName) + ' ' + %trim(EMNam2);
             //*****   DOPEOFdnm = %trim(EMFdnm) + ' ' + %trim(EMFdn2);
             //***   DOPEOName = EMName + EMNam2;
             //***   DOPEOFdnm = EMFdnm + EMFdn2;
             //*** end chg *****  04/21/21  *********************************
             //***   DOPEOFein = EMFein;
             //*** begin add ***  01/15/18  ******************************
             //***   If xxSeq# <> 0;
             //***      Chain(n) (keyCo#:keyFnd:1:0:xxSeq#) pan01;
             //***      If %found;
             //*** begin chg ***  04/21/21  *********************************
             //*****  DOPEOName = %trim(ANName) + ' ' + %trim(ANNam2);
             //***         DOPEOName = ANName + ANNam2;
             //*** end chg *****  04/21/21  *********************************
             //***        DOPEOFein = ANFein;
             //***        Clear DOPEOFdnm;
             //***      Endif;
             //***   Endif;
             //*** end add *****  01/15/18  ******************************
             //***   If DOEmMAd1 <> ' ';
             //***      DOPEOAdr1 = EMMAd1;
             //***      DOPEOAdr2 = EMMAd2;
             //***      DOPEOCity = EMMCty;
             //***      DOPEOSt   = EMMSt;
             //***      DOPEOZip  = EMMZip;
             //***   Else;
             //***      DOPEOAdr1 = EMPAd1;
             //***      DOPEOAdr2 = EMPAd2;
             //***      DOPEOCity = EMPCty;
             //***      DOPEOSt   = EMPSt;
             //***      DOPEOZip  = EMPZip;
             //***   Endif;
             //*** end chg *****  03/01/17  ******************************
             //***Endif;
          //***Endif;

       //***Endif;

       //***Chain (keyCo#:keyFnd) pf200;
       //***If %found;
       //***   DOCar# = F2Car#;
       //*** begin add ***  03/08/17  ******************************************
       //***   DONAIC = F2NAIC;
       //*** end add *****  03/08/17  ******************************************
       //***  DOCARFEIN = F2FEIN;
       //***Endif;
       //*** end add *****  10/13/16  ******************************************

       //*** begin add ***  10/12/17  **********************************
       //***stRptDiv = keyDiv;
       //***Setll (keyCo#:keyFnd:keyEmp#) pem00;
       //***Reade(n) (keyCo#:keyFnd:keyEmp#) pem00;
       //***Dow not %eof;
       //***   If EMStrp = 'Y';
       //***      stRptDiv = EMDiv;
       //***      Leave;
       //***   Endif;
       //***   Reade(n) (keyCo#:keyFnd:keyEmp#) pem00;
       //***Enddo;
       //*** begin add ***  03/05/18  *********************************
       //***Clear xxNJSt;
       //***Setll (keyCo#:keyFnd:keyEmp#) pem00;
       //***Reade(n) (keyCo#:keyFnd:keyEmp#) pem00;
       //***Dow not %eof;
       //***   Chain (keyCo#:keyFnd:keyEmp#:EMDiv:keyFyr) pel00;
       //***   If %found;
       //***      xxKey = %EditC(ELbscd:'X');
       //***      Chain keyWTABL pbs01;
       //***      If %found and RSJrst = 'NJ';
       //***         xxNJSt = 'Y';
       //***         Leave;
       //***      Endif;
       //***   Endif;
       //***   Reade(n) (keyCo#:keyFnd:keyEmp#) pem00;
       //***Enddo;

       //***Chain keyWMEML pem00;
       //***Chain keyWDELL pel00;
       //*** end add *****  03/05/18  *********************************


       //***##co#  = %editc(keyCo#:'X');
       //***##fnd  = %editc(keyFnd:'X');
       //***##emp# = %editc(keyEmp#:'X');
       //***##div  = %editc(keyDiv:'X');
       //***##odr  = ' ';
       //*** begin chg ***  02/12/24  **********************************
       //*****##nodr = 'F';
       //*****##aodr = 'M';
       //***##nodr = ' ';
       //***##aodr = ' ';
       //***If XPType = 'Policy Page' or XPType = 'Cancellations' or
       //*** XPType = 'Reinstatements' or XPType = 'Mid-Term Endorsement' or
       //*** XPType = 'Certificate of Insurance';
       //***   ##nodr = 'F';
       //***   ##aodr = 'M';
       //***Endif;
       //*** end chg *****  02/12/24  **********************************
       //***##emn1 = ' ';
       //***##emn2 = ' ';
       //***##emn4 = ' ';
       //***##emn5 = ' ';
       //***##emn6 = ' ';
       //***##emn7 = ' ';
       //***##adrl = 0;
       //***##naml = 0;
       //***##fedl = 0;
       //***##peo1 = ' ';
       //***##peo2 = ' ';
       //***##pgfl = ' ';
       //***##peoext = ' ';
       //***If FNPEOF <> ' ';
       //***  ##pgfl = DOPEOPGFL;
       //***  ##peoext = DOPEOAcro;
       //*** begin add ***  03/05/18  *********************************
       //***   If ##pgfl = ' ';
       //***      ##pgfl = 'B';
       //***   Endif;
       //*** end add *****  03/05/18  *********************************
       //*** begin add ***  05/01/18  *********************************
       //***   If DORateSt = 'CA';
       //***      If ##pgfl = 'B';
       //***         ##pgfl = 'C';
       //***      Endif;
       //***      If ##pgfl = '3';
       //***         ##pgfl = 'E';
       //***      Endif;
       //***   Endif;
       //*** end add *****  05/01/18  *********************************
       //***Endif;
       //***##conl = 0;
       //***##amn1 = ' ';
       //***##amn2 = ' ';
       //***##amn4 = ' ';
       //***##amn5 = ' ';
       //***##amn6 = ' ';
       //***##amn7 = ' ';
       //***##amn8 = ' ';
       //***##amn9 = ' ';
       //***##peol = 0;
       //***##dbaf = ' ';
       //*** begin add ***  03/05/18  *********************************
       //***##dbaf = xxNJSt;
       //*** end add *****  03/05/18  *********************************
       //***##al2 = 0;
       //***##nl2 = 0;
       //***##fl2 = 0;
       //***##pl2 = 0;
       //***##cl2 = 0;
       //***##name1 = ' ';
       //***##name2 = ' ';
       //***##name3 = ' ';
       //***##name4 = ' ';
       //*** Begin Add *** 02/22/22 ****************************************
       //***##fein = 0;
       //*** End   Add *** 02/22/22 ****************************************

       //*** Begin Chg *** 02/22/22 ****************************************
       //*** SrAddr(##co#:##fnd:##emp#:##div:##odr:##nodr:##aodr:##emn1:
       //***  ##emn2:##emn3:##emn4:##emn5:##emn6:##emn7:##adrl:##naml:
       //***  ##fedl:##peo1:##peo2:##pgfl:##conl:##amn1:##amn2:##amn3:
       //*** ##amn4:##amn5:##amn6:##amn7:##amn8:##amn9:##peol:##dbaf:##al2:
       // ##nl2:##fl2:##pl2:##cl2:##peoext:##name1:##name2:##name3:##name4);
       //*** begin chg ***  02/12/24  **********************************
       //***** GetNameAddress(keyCo#:keyFnd:keyEmp#:keydiv:keyFyr:##emn1:
       //***** ##emn2:##emn3:##emn4:##emn5:##emn6:##emn7:##adrl:##naml:
       //***** ##fedl:##peo1:##peo2:##conl:##amn1:##amn2:##amn3:
       //***** ##amn4:##amn5:##amn6:##amn7:##amn8:##amn9:##peol:##al2:
       //***** ##nl2:##fl2:##pl2:##cl2:##name1:##name2:##name3:##name4:##fein);
       //*** GetNameAddress(keyCo#:keyFnd:keyEmp#:keydiv:keyFyr:##emn1:##emn2:
       //*** ##emn3:##emn4:##emn5:##emn6:##emn7:##adrl:##naml:##fedl:##peo1:
       //***##peo2:##conl:##amn1:##amn2:##amn3:##amn4:##amn5:##amn6:##amn7:
       //*** ##amn8:##amn9:##peol:##al2:##nl2:##fl2:##pl2:##cl2:##name1:
       //*** ##name2:##name3:##name4:##fein:##odr:##nodr:##aodr);
       //*** end chg *****  02/12/24  **********************************
       //*** End   Chg *** 02/22/22 ****************************************

       //***DODivNam1 = ##Name1;
       //***DODivNam2 = ##Name2;
       //***DODivNam3 = ##Name3;
       //***DODivNam4 = ##Name4;
       //*** begin add ***  03/26/18  ************************************
       //***If ##adrl > 0;
       //*** end add *****  03/26/18  ************************************
       //***   DODivAdr1 = %trim(amn(##adrl));
       //***   DODivAdr2 = %trim(amn(##adrl+1));
       //***   DODivAdr3 = %trim(amn(##adrl+2));
       //*** begin add ***  03/26/18  ************************************
       //***Endif;
       //*** end add *****  03/26/18  ************************************

       //*** Begin Chg *** 05/17/24 **************************************
       //*** If stRptDiv <> keyDiv;
       //***If stRptDiv <> keyDiv And  isCalledFromBilling = *Off;
       //*** End   Chg *** 05/17/24 **************************************

       //***   ##co#  = %editc(keyCo#:'X');
       //***   ##fnd  = %editc(keyFnd:'X');
       //***   ##emp# = %editc(keyEmp#:'X');
       //***   ##div  = %editc(stRptDiv:'X');
       //***   ##odr  = ' ';
       //*** begin chg ***  02/12/24  **********************************
          //*****##nodr = 'F';
          //*****##aodr = 'M';
       //***   ##nodr = ' ';
       //***   ##aodr = ' ';
       //***   If XPType = 'Policy Page' or XPType = 'Cancellations' or
       //***    XPType = 'Reinstatements' or XPType = 'Mid-Term Endorsement' or
       //***    XPType = 'Certificate of Insurance';
       //***      ##nodr = 'F';
       //***     ##aodr = 'M';
       //***   Endif;
       //*** end chg *****  02/12/24  **********************************
       //***   ##emn1 = ' ';
       //***   ##emn2 = ' ';
       //***   ##emn4 = ' ';
       //***   ##emn5 = ' ';
       //***   ##emn6 = ' ';
       //***   ##emn7 = ' ';
       //***   ##adrl = 0;
       //***   ##naml = 0;
       //***   ##fedl = 0;
       //***   ##peo1 = ' ';
       //***   ##peo2 = ' ';
       //***   ##pgfl = ' ';
       //***   ##peoext = ' ';
       //***   If FNPEOF <> ' ';
       //***      ##pgfl = DOPEOPGFL;
       //***      ##peoext = DOPEOAcro;
          //*** begin add ***  03/05/18  *********************************
       //***      If ##pgfl = ' ';
       //***         ##pgfl = 'B';
       //***      Endif;
          //*** end add *****  03/05/18  *********************************
          //*** begin add ***  05/01/18  *********************************
       //***      If DORateSt = 'CA';
       //***         If ##pgfl = 'B';
       //***            ##pgfl = 'C';
       //***         Endif;
       //***         If ##pgfl = '3';
       //***            ##pgfl = 'E';
       //***         Endif;
       //***      Endif;
          //*** end add *****  05/01/18  *********************************
       //***   Endif;
       //***   ##conl = 0;
       //***   ##amn1 = ' ';
       //***   ##amn2 = ' ';
       //***   ##amn4 = ' ';
       //***   ##amn5 = ' ';
       //***   ##amn6 = ' ';
       //***   ##amn7 = ' ';
       //***   ##amn8 = ' ';
       //***   ##amn9 = ' ';
       //***   ##peol = 0;
       //***   ##dbaf = ' ';
          //*** begin add ***  03/05/18  *********************************
        //***  ##dbaf = xxNJSt;
          //*** end add *****  03/05/18  *********************************
        //***  ##al2 = 0;
        //***  ##nl2 = 0;
        //***  ##fl2 = 0;
        //***  ##pl2 = 0;
        //***  ##cl2 = 0;
        //***  ##name1 = ' ';
        //***  ##name2 = ' ';
        //***  ##name3 = ' ';
        //***  ##name4 = ' ';
          //*** Begin Add *** 02/22/22 *************************************
        //***  ##fein = 0;
          //*** End   Add *** 02/22/22 *************************************

          //*** Begin Chg *** 02/22/22 *************************************
          //***SrAddr(##co#:##fnd:##emp#:##div:##odr:##nodr:##aodr:##emn1:
          //*** ##emn2:##emn3:##emn4:##emn5:##emn6:##emn7:##adrl:##naml:
          //*** ##fedl:##peo1:##peo2:##pgfl:##conl:##amn1:##amn2:##amn3:
          //* ##amn4:##amn5:##amn6:##amn7:##amn8:##amn9:##peol:##dbaf:##al2:
          //##nl2:##fl2:##pl2:##cl2:##peoext:##name1:##name2:##name3:##name4);
          //*** begin chg ***  02/12/24  **********************************
          //***** GetNameAddress(keyCo#:keyFnd:keyEmp#:stRptDiv:keyFyr:##emn1:
          //***** ##emn2:##emn3:##emn4:##emn5:##emn6:##emn7:##adrl:##naml:
          //***** ##fedl:##peo1:##peo2:##conl:##amn1:##amn2:##amn3:
          //***** ##amn4:##amn5:##amn6:##amn7:##amn8:##amn9:##peol:##al2:
          //***** ##nl2:##fl2:##pl2:##cl2:##name1:##name2:##name3:##name4:##fein);
          //*** GetNameAddress(keyCo#:keyFnd:keyEmp#:stRptDiv:keyFyr:##emn1:##emn2:
          //*** ##emn3:##emn4:##emn5:##emn6:##emn7:##adrl:##naml:##fedl:##peo1:
          //*** ##peo2:##conl:##amn1:##amn2:##amn3:##amn4:##amn5:##amn6:##amn7:
          //*** ##amn8:##amn9:##peol:##al2:##nl2:##fl2:##pl2:##cl2:##name1:
          //*** ##name2:##name3:##name4:##fein:##odr:##nodr:##aodr);
          //*** end chg *****  02/12/24  **********************************
          //*** End   Chg *** 02/22/22 *************************************

          //***DOPolNam1 = ##Name1;
          //***DOPolNam2 = ##Name2;
          //***DOPolNam3 = ##Name3;
          //***DOPolNam4 = ##Name4;
          //***DOPolAdr1 = %trim(amn(##adrl));
          //***DOPolAdr2 = %trim(amn(##adrl+1));
          //***DOPolAdr3 = %trim(amn(##adrl+2));
       //***Else;
          //***DOPolNam1 = DODivNam1;
          //***DOPolNam2 = DODivNam2;
          //***DOPolNam3 = DODivNam3;
          //***DOPolNam4 = DODivNam4;
          //***DOPolAdr1 = DODivAdr1;
          //***DOPolAdr2 = DODivAdr2;
          //***DOPolAdr3 = DODivAdr3;
       //***Endif;

       //*** end add *****  10/12/17  **********************************

       //* Maintenance info.
       //***DOPgm  = Q1PGM;
       //***DOUser = Q1USER;
       //***DOTime = Timeto6(SystemTime());
       //***DODt   = Dateto7(SystemDate());

       //* Write workfile record.
       //***Write pdo0p;
       //*** End Del ***** 02/04/25 *********************************************
       //*** begin add ***  11/16/16  **************************************************
          Endif;
       //*** end add *****  11/16/16  **************************************************

       //*** begin add ***  05/31/16  **************************************************
          If XPLink = ' ';
             xxProcess = ' ';
          Else;
       //*** begin add ***  10/04/16  **************************************************
             xxProcess = ' ';
       //*** end add *****  10/04/16  **************************************************
             Reade(n) key1XML xp100;
             If not %eof;
                xxProcess = 'Y';
             Endif;
          Endif;

       Enddo;
       //*** end add *****  05/31/16  **************************************************

      /end-free
     C*
     C* Exit program.
     C*
     C                   Eval      *inLR = *on
     C                   Return
     C*
      //*** Beg Add ***** 02/04/25 *********************************************
     C**************************************************************************************
     C* Create Work file Data
     C**************************************************************************************
       Begsr WriteSXDOP;

        curDate = Dateto7(SystemDate());

        //* Clear record format for workfile and populate key fields.
        Clear pdo0p;

        DOXml  = strXmlT;
        DOCo#  = keyCo#;
        DOFnd  = keyFnd;
        DOEmp# = keyEmp#;
        DODiv  = keyDiv;
        DOFyr  = keyFyr;
        DOCase = keyCase;

        //* Populate fields associated with each area of work file.
        Exsr Company;
        Exsr Group;
        Exsr Employer;
        Exsr PolicyYear;
        Exsr Agent;
        Exsr Contacts;

        //* Populate claim data if needed.
        If XPCat = 'CS';
           Exsr Claim;
           Exsr Trials;
           Exsr Payments;
        Endif;

        DOEnviro = CheckEnv();

        DOPolHdg  = 'Carrier Policy #:';
        DOPPolHdg = 'Carrier Prior Policy #:';
        DONJTaxHd = 'N.J. Taxpayer Identification No.:';
        DOFileHdg = 'File #:';
        DOPEOTaxH = 'L/C/F N.J. Tax Id No.:';

        Chain (keyCo#:keyFnd:keyEmp#:keyDiv:keyFyr-1) pel00;
        If %found;
           DOPPol# = %editc(keyFnd:'X') + %editc(keyEmp#:'X') +
            %editc(keyFyr-1:'X');
            Chain (keyCo#:keyFnd:keyEmp#:keyDiv:keyFyr-1) prp00;
           If %found;
              DOPPOL# = %trim(RPCap#);
           Endif;
        Else;
           DOPPol#   = 'New';
        Endif;
        // Get the original WDELP record for the Fyr being processed.
        Chain keyWDELL pel00;

        Chain (keyCo#:ELRTCD) prs00;
        If %found;
           DORateSt = RSJRST;
        Endif;
        If %found and RSJRST = 'NJ';
           If clientID = '114';
              DONJTaxId = %trim(EMUE#);
           Else;
              DONJTaxId = %trim(EMSEMP);
           Endif;
        Endif;

        //* Set the policy limits
        If ELIlcd = *blank;
           Setgt (keyCo#:keyFnd:DoRateSt:ELPfdt) prr00;
           Readpe (keyCo#:keyFnd:DoRateSt) prr00;
           If not %eof;
              DOLmtAcc = 1000 * RRAcc;
              DOLmtDis = 1000 * RRDis;
              DOLmtAgg = 1000 * RRAgg;
           Endif;
        Else;
           If ELInl2 <> ' ' and ELI2ef <= curDate;
              xxKey  = ELInl2;
           Else;
              xxKey  = ELIlcd;
           Endif;
           Setgt (keyCo#:keyFnd:DORateSt:xxKey:ELPfdt) plm01;
           Readpe(n) (keyCo#:keyFnd:DORateSt:xxKey) plm01;
           If not %eof;
              DOLmtAcc = 1000 * LMAcc;
              DOLmtDis = 1000 * LMDis;
              DOLmtAgg = 1000 * LMAgg;
           Endif;
        Endif;

        //* Populate cancellation reason code/description if cancelled.
        If DOEMCxdt <> 0 and DORateSt <> '  ';
           Chain(n) (EMCo#:EMFnd:EMEmp#:EMDiv:DOEmCxdt) pcx01;
           If %found;
              DOCxRsn = CXRsn;
              xxKey = CXRSN;
              Setgt (EMCo#:DORateSt:xxKey:curDate) pcr00;
              Readpe(n) (EMCo#:DoRateSt:xxKey) pcr00;
              Dow not %eof and CRStat = 'I';
                 Readpe(n) (EMCo#:DoRateSt:xxKey) pcr00;
              Enddo;
              If not %eof;
                 DOCxRsnD = CRDesc;
              Endif;
           Endif;
        Endif;

        If FNPEOF <> ' ';
           Setgt (keyCo#:keyFnd:DoRateSt:ELPfdt) prr00;
           Readpe (keyCo#:keyFnd:DoRateSt) prr00;
           If not %eof;
              DOPEOPGFL = RRPGFL;
              DOPEOAcro = RRPEOA;
              If FNPEOF = 'C' and RRPEOF <> '' ;
                 DOFnPEOF = RRPEOF;
              Endif;
           Endif;
           If keyEmp# <> 1 and (FNPEOF = 'M' or FNPEOF = 'Y' or FNPEOF = 'C');
              xxSeq# = EMPEONMIN;
              Chain (keyCo#:keyFnd:1:0) pem00;
              If %found;
                 DOPEOName = EMName + EMNam2;
                 DOPEOFdnm = EMFdnm + EMFdn2;
                 DOPEOFein = EMFein;
                 If xxSeq# <> 0;
                    Chain(n) (keyCo#:keyFnd:1:0:xxSeq#) pan01;
                    If %found;
                       DOPEOName = ANName + ANNam2;
                       DOPEOFein = ANFein;
                       Clear DOPEOFdnm;
                    Endif;
                 Endif;
                 If DOEmMAd1 <> ' ';
                    DOPEOAdr1 = EMMAd1;
                    DOPEOAdr2 = EMMAd2;
                    DOPEOCity = EMMCty;
                    DOPEOSt   = EMMSt;
                    DOPEOZip  = EMMZip;
                 Else;
                    DOPEOAdr1 = EMPAd1;
                    DOPEOAdr2 = EMPAd2;
                    DOPEOCity = EMPCty;
                    DOPEOSt   = EMPSt;
                    DOPEOZip  = EMPZip;
                 Endif;
              Endif;
           Endif;
        Endif;

        Chain (keyCo#:keyFnd) pf200;
        If %found;
           DOCar# = F2Car#;
           DONAIC = F2NAIC;
           DOCARFEIN = F2FEIN;
        Endif;

        stRptDiv = keyDiv;
        Setll (keyCo#:keyFnd:keyEmp#) pem00;
        Reade(n) (keyCo#:keyFnd:keyEmp#) pem00;
        Dow not %eof;
           If EMStrp = 'Y';
              stRptDiv = EMDiv;
              Leave;
           Endif;
           Reade(n) (keyCo#:keyFnd:keyEmp#) pem00;
        Enddo;
        Clear xxNJSt;
        Setll (keyCo#:keyFnd:keyEmp#) pem00;
        Reade(n) (keyCo#:keyFnd:keyEmp#) pem00;
        Dow not %eof;
           Chain (keyCo#:keyFnd:keyEmp#:EMDiv:keyFyr) pel00;
           If %found;
              xxKey = %EditC(ELbscd:'X');
              Chain keyWTABL pbs01;
              If %found and RSJrst = 'NJ';
                 xxNJSt = 'Y';
                 Leave;
              Endif;
           Endif;
           Reade(n) (keyCo#:keyFnd:keyEmp#) pem00;
        Enddo;

        Chain keyWMEML pem00;
        Chain keyWDELL pel00;

        ##co#  = %editc(keyCo#:'X');
        ##fnd  = %editc(keyFnd:'X');
        ##emp# = %editc(keyEmp#:'X');
        ##div  = %editc(keyDiv:'X');
        ##odr  = ' ';
        ##nodr = ' ';
        ##aodr = ' ';
        If XPType = 'Policy Page' or XPType = 'Cancellations' or
         XPType = 'Reinstatements' or XPType = 'Mid-Term Endorsement' or
         XPType = 'Certificate of Insurance';
           ##nodr = 'F';
           ##aodr = 'M';
        Endif;
        ##emn1 = ' ';
        ##emn2 = ' ';
        ##emn4 = ' ';
        ##emn5 = ' ';
        ##emn6 = ' ';
        ##emn7 = ' ';
        ##adrl = 0;
        ##naml = 0;
        ##fedl = 0;
        ##peo1 = ' ';
        ##peo2 = ' ';
        ##pgfl = ' ';
        ##peoext = ' ';
        If FNPEOF <> ' ';
           ##pgfl = DOPEOPGFL;
           ##peoext = DOPEOAcro;
           If ##pgfl = ' ';
              ##pgfl = 'B';
           Endif;
           If DORateSt = 'CA';
              If ##pgfl = 'B';
                 ##pgfl = 'C';
              Endif;
              If ##pgfl = '3';
                 ##pgfl = 'E';
              Endif;
           Endif;
        Endif;
        ##conl = 0;
        ##amn1 = ' ';
        ##amn2 = ' ';
        ##amn4 = ' ';
        ##amn5 = ' ';
        ##amn6 = ' ';
        ##amn7 = ' ';
        ##amn8 = ' ';
        ##amn9 = ' ';
        ##peol = 0;
        ##dbaf = ' ';
        ##dbaf = xxNJSt;
        ##al2 = 0;
        ##nl2 = 0;
        ##fl2 = 0;
        ##pl2 = 0;
        ##cl2 = 0;
        ##name1 = ' ';
        ##name2 = ' ';
        ##name3 = ' ';
        ##name4 = ' ';
        ##fein = 0;

        GetNameAddress(keyCo#:keyFnd:keyEmp#:keydiv:keyFyr:##emn1:##emn2:
        ##emn3:##emn4:##emn5:##emn6:##emn7:##adrl:##naml:##fedl:##peo1:
        ##peo2:##conl:##amn1:##amn2:##amn3:##amn4:##amn5:##amn6:##amn7:
        ##amn8:##amn9:##peol:##al2:##nl2:##fl2:##pl2:##cl2:##name1:
        ##name2:##name3:##name4:##fein:##odr:##nodr:##aodr);

        DODivNam1 = ##Name1;
        DODivNam2 = ##Name2;
        DODivNam3 = ##Name3;
        DODivNam4 = ##Name4;
        If ##adrl > 0;
           DODivAdr1 = %trim(amn(##adrl));
           DODivAdr2 = %trim(amn(##adrl+1));
           DODivAdr3 = %trim(amn(##adrl+2));
        Endif;

        If stRptDiv <> keyDiv And  isCalledFromBilling = *Off;
           ##co#  = %editc(keyCo#:'X');
           ##fnd  = %editc(keyFnd:'X');
           ##emp# = %editc(keyEmp#:'X');
           ##div  = %editc(stRptDiv:'X');
           ##odr  = ' ';
           ##nodr = ' ';
           ##aodr = ' ';
           If XPType = 'Policy Page' or XPType = 'Cancellations' or
            XPType = 'Reinstatements' or XPType = 'Mid-Term Endorsement' or
            XPType = 'Certificate of Insurance';
              ##nodr = 'F';
              ##aodr = 'M';
           Endif;
           ##emn1 = ' ';
           ##emn2 = ' ';
           ##emn4 = ' ';
           ##emn5 = ' ';
           ##emn6 = ' ';
           ##emn7 = ' ';
           ##adrl = 0;
           ##naml = 0;
           ##fedl = 0;
           ##peo1 = ' ';
           ##peo2 = ' ';
           ##pgfl = ' ';
           ##peoext = ' ';
           If FNPEOF <> ' ';
              ##pgfl = DOPEOPGFL;
              ##peoext = DOPEOAcro;
              If ##pgfl = ' ';
                 ##pgfl = 'B';
              Endif;
              If DORateSt = 'CA';
                 If ##pgfl = 'B';
                    ##pgfl = 'C';
                 Endif;
                 If ##pgfl = '3';
                    ##pgfl = 'E';
                 Endif;
              Endif;
           Endif;
           ##conl = 0;
           ##amn1 = ' ';
           ##amn2 = ' ';
           ##amn4 = ' ';
           ##amn5 = ' ';
           ##amn6 = ' ';
           ##amn7 = ' ';
           ##amn8 = ' ';
           ##amn9 = ' ';
           ##peol = 0;
           ##dbaf = ' ';
           ##dbaf = xxNJSt;
           ##al2 = 0;
           ##nl2 = 0;
           ##fl2 = 0;
           ##pl2 = 0;
           ##cl2 = 0;
           ##name1 = ' ';
           ##name2 = ' ';
           ##name3 = ' ';
           ##name4 = ' ';
           ##fein = 0;

           GetNameAddress(keyCo#:keyFnd:keyEmp#:stRptDiv:keyFyr:##emn1:##emn2:
           ##emn3:##emn4:##emn5:##emn6:##emn7:##adrl:##naml:##fedl:##peo1:
           ##peo2:##conl:##amn1:##amn2:##amn3:##amn4:##amn5:##amn6:##amn7:
           ##amn8:##amn9:##peol:##al2:##nl2:##fl2:##pl2:##cl2:##name1:
           ##name2:##name3:##name4:##fein:##odr:##nodr:##aodr);

           DOPolNam1 = ##Name1;
           DOPolNam2 = ##Name2;
           DOPolNam3 = ##Name3;
           DOPolNam4 = ##Name4;
           DOPolAdr1 = %trim(amn(##adrl));
           DOPolAdr2 = %trim(amn(##adrl+1));
           DOPolAdr3 = %trim(amn(##adrl+2));
        Else;
           DOPolNam1 = DODivNam1;
           DOPolNam2 = DODivNam2;
           DOPolNam3 = DODivNam3;
           DOPolNam4 = DODivNam4;
           DOPolAdr1 = DODivAdr1;
           DOPolAdr2 = DODivAdr2;
           DOPolAdr3 = DODivAdr3;
        Endif;

        //* Maintenance info.
        DOPgm  = Q1PGM;
        DOUser = Q1USER;
        DOTime = Timeto6(SystemTime());
        DODt   = Dateto7(SystemDate());

        //* Write workfile record.
        Write pdo0p;
       Endsr;
       //*** End Add ***** 02/04/25 *********************************************

     C**************************************************************************************
     C*
     C* Company
     C*
     C**************************************************************************************
     C     Company       Begsr
      /free

       Chain keyCO# pco00;
       If %found;

          DOCoName = COName;
          DOCoAdr1 = COAdr1;
          DOCoAdr2 = COAdr2;
          DOCoCty  = COCty;
          DOCoSt   = COSt;
          DOCoZip  = COZip;
          DOCoFein = COFein;
          //*** begin add ***  07/31/15  *************************************
          DOCoMad1 = COMad1;
          DOCoMad2 = COMad2;
          DOCoMCty = COMCty;
          DOCoMSt  = COMSt;
          DOCoMZip = COMZip;
          DOCoPhn# = COPhn#;
          DOCoArea = COArea;
          DOCoFax# = COFax#;
          DOCoFAra = COFAra;
          //*** end add *****  07/31/15  *************************************
          //*** Beg Add ***** 05/08/25 ***************************************
          DOCoMail = COMail;
          //*** End Add ***** 05/08/25 ***************************************

       Endif;

      /end-free
     C                   Endsr                                                  Company subr
     C**************************************************************************************
     C*
     C* Group
     C*
     C**************************************************************************************
     C     Group         Begsr
      /free

       Chain (keyCo#:keyFnd) pfn00;
       If %found;

          DOFnName = FNName;
          DOFnShNm = FNShNm;
          DOFnANam = FNANam;
          DOFnMAd1 = FNMAd1;
          DOFnMAd2 = FNMAd2;
          DOFnMCty = FNMCty;
          DOFnMSt  = FNMSt;
          DOFnMZip = FNMZip;
          DOFnPAd1 = FNPAd1;
          DOFnPAd2 = FNPAd2;
          DOFnPCty = FNPCty;
          DOFnPSt  = FNPSt;
          DOFnPZip = FNPZip;
          DOFnAAd1 = FNAAd1;
          DOFnAAd2 = FNAAd2;
          DOFnACty = FNACty;
          DOFnASt  = FNASt;
          DOFnAZip = FNAZip;
          DOFnPh1A = FNPh1A;
          DOFnPh1  = FNPh1;
          DOFnArea = FNArea;
          DOFnPh2  = FNPh2;
          DOFnFaxA = FNFaxA;
          DOFnFax  = FNFax;
          DOFnIAdr = FNIAdr;
          DOTstGrp = FNTest;
          //*** begin add ***  01/10/17  ************************************
          DOFnCGFL = FNCGFL;
          DOFnPEOF = FNPEOF;
          //*** end add *****  01/10/17  ************************************
          //*** Beg Add ***** 05/08/25 ***************************************
          DOFnAmail = FNAmail;
          DOFnAara = FNAara;
          DOFnAphn = FNAphn;
          //*** End Add ***** 05/08/25 ***************************************
          //*** begin add *** 09/26/25 ***************************************
          DOFnASOF = FNASOF;
          //*** end add ***** 09/26/25 ***************************************

       Endif;

      /end-free
     C                   Endsr                                                  Group subr
     C**************************************************************************************
     C*
     C* Employer
     C*
     C**************************************************************************************
     C     Employer      Begsr
      /free

       //*** begin chg ***  04/21/21  *********************************
       //*****DOEmName = %trim(EMName) + ' ' + %trim(EMNam2);
       //*****DOEmFdnm = %trim(EMFdnm) + ' ' + %trim(EMFdn2);
       DOEmName = EMName + EMNam2;
       DOEmFdnm = EMFdnm + EMFdn2;
       //*** end chg *****  04/21/21  *********************************
       DOEmFein = EMFein;
       DOEmMAd1 = EMMAd1;
       DOEmMAd2 = EMMAd2;
       DOEmMCty = EMMCty;
       DOEmMSt  = EMMSt;
       DOEmMZip = EMMZip;
       DOEmPAd1 = EMPAd1;
       DOEmPAd2 = EMPAd2;
       DOEmPCty = EMPCty;
       DOEmPSt  = EMPSt;
       DOEmPZip = EMPZip;
       //*** begin add ***  01/10/17  ************************************
       DOEmMPFL = EMMPFL;
       //*** end add *****  01/10/17  ************************************
       //*** begin add ***  03/26/18  ************************************
       DONatoBus= EMNAOB;
       //*** end add *****  03/26/18  ************************************
       //*** Begin Add *** 05/10/23 **************************************
       // Type of business description
       Select;
       When EMtpOB = '4' And EMnaOB <> ' ';
          DOtypeBus = EMnaOB;
       When EMtpOB <> ' ';
          xxTbID = 'BUSI';
          xxKey = EMtpOB;
          DOtypeBus = ' ';
          Chain (EMco#:xxTbID:xxKey) pgt00;
          If %Found(wtgtl);
             DOtypeBus = GTdesc;
          EndIf;
       Other;
          DOtypeBus = ' ';
       EndSL;
       //*** End   Add *** 05/10/23 **************************************
       //* Pull Country description based on Physical or Mailing state.
       If EMPSt <> *Blanks;
          xxKey = %Trim(EMPSt) + %Trim(EMCnty);
       Else;
          xxKey = %Trim(EMMst) + %Trim(EMCnty);
       EndIf;

       Chain (keyCo#:xxKey) pcn01;

       If %found;
          DOCounty = %trim(CnDesc);
       Endif;

       //* Pull Region description.
       Chain (keyCo#:keyFnd:EMRgn#) pre00;

       If %found;
          DORegion = %trim(REDesc);
       Endif;

       //* Pull Chapter description.
       Chain (keyCo#:keyFnd:EMChap) pcp00;

       If %found;
          DOChaptr = %trim(CPDesc);
       Endif;

       DOEmArea = EMArea;
       DOEmPhn# = EMPhn#;
       DOEmFaxa = EMFaxa;
       DOEmFax# = EMFax#;
       DOEmIadr = EMIadr;
       DOEmRsk# = EMRsk#;
       DOEmSemp = EMSemp;
       DOEmUE#  = EMUE#;

       //* SIC code
       DOEMSic  = EMSic;

       xxTbid = 'SIC';
       xxkey = EMSic;
       Chain keyWTGTL pgt00;
       If %found;
          DOSicDes = %trim(GTDesc);
       Endif;

       //* NAICS code
       DOEMNaic = EMNaic;

       xxTbid = 'NAIC';
       xxkey = EMNaic;
       Chain keyWTGTL pgt00;
       If %found;
          DONaiDes = %trim(GTDesc);
       Endif;

       DOEmEfdt = EMEfd1;
       DOEmCxdt = GetCxDate(keyCo#:keyFnd:keyEmp#:keyDiv);

       //* Policy Services Rep
       If EMPsrv <> ' ';
          Chain(n) EMPsrv pup01;
          If %found;
             DOPsName = UPUsnm;
             DOPsTitl = UPTitl;
             DOPsExt# = UPExt#;
             DOPsMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOPsPhnA = UPPhnA;
             DOPsPhn# = UPPhn#;
             DOPsFaxA = UPFaxA;
             DOPsFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Underwriter
       If EMUwtr <> ' ';
          Chain(n) EMUwtr pup01;
          If %found;
             DOUwName = UPUsnm;
             DOUwTitl = UPTitl;
             DOUwExt# = UPExt#;
             DOUwMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOUwPhnA = UPPhnA;
             DOUwPhn# = UPPhn#;
             DOUwFaxA = UPFaxA;
             DOUwFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Premium Accountant
       If EMPacc <> ' ';
          Chain(n) EMPacc pup01;
          If %found;
             DOPaName = UPUsnm;
             DOPaTitl = UPTitl;
             DOPaExt# = UPExt#;
             DOPaMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOPaPhnA = UPPhnA;
             DOPaPhn# = UPPhn#;
             DOPaFaxA = UPFaxA;
             DOPaFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Audit Reviewer
       If EMArvw <> ' ';
          Chain(n) EMArvw pup01;
          If %found;
             DOArName = UPUsnm;
             DOArTitl = UPTitl;
             DOArExt# = UPExt#;
             DOArMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOArPhnA = UPPhnA;
             DOArPhn# = UPPhn#;
             DOArFaxA = UPFaxA;
             DOArFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Audit Manager
       If EMAmgr <> ' ';
          Chain(n) EMAmgr pup01;
          If %found;
             DOAmName = UPUsnm;
             DOAmTitl = UPTitl;
             DOAmExt# = UPExt#;
             DOAmMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOAmPhnA = UPPhnA;
             DOAmPhn# = UPPhn#;
             DOAmFaxA = UPFaxA;
             DOAmFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Loss Control
       If EMLrep <> ' ';
          Chain(n) EMLrep pup01;
          If %found;
             DOLcName = UPUsnm;
             DOLcTitl = UPTitl;
             DOLcExt# = UPExt#;
             DOLcMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOLcPhnA = UPPhnA;
             DOLcPhn# = UPPhn#;
             DOLcFaxA = UPFaxA;
             DOLcFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Claim Contact
       If EMClmc <> ' ';
          Chain(n) EMClmc pup01;
          If %found;
             DOCcName = UPUsnm;
             DOCcTitl = UPTitl;
             DOCcExt# = UPExt#;
             DOCcMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOCcPhnA = UPPhnA;
             DOCcPhn# = UPPhn#;
             DOCcFaxA = UPFaxA;
             DOCcFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Billing Address
       Chain keyWMA3L pa300;
       If %found;

          DOA3Adr1 = A3Adr1;
          DOA3Adr2 = A3Adr2;
          DOA3Cty  = A3Cty;
          DOA3St   = A3St;
          //*** begin chg ***  01/05/16  ***********************************
          //*****DOA3Zip  = %int(A3Zip1 + A3Zip2);
          Monitor;
             DOA3Zip  = %int(A3Zip1 + A3Zip2);
          On-error;
             DOA3Zip = 0;
          Endmon;
          //*** end chg *****  01/05/16  ***********************************
          DOA3Ooc  = A3Ooc;
          DOA3Cnt  = A3Cont;

       Endif;

       //*** begin add ***  01/07/16  ***********************************
       //* Employer Office
       Chain (keyCo#:EMOff#) pof00;
       If %found;

          DOOfName = OFName;
          DOOfAdr1 = OFAdr1;
          DOOfAdr2 = OFAdr2;
          DOOfCty  = OFCty;
          DOOfSt   = OFSt;
          DOOfZip  = OFZip;
          DOOfArea = OFArea;
          DOOfPhn# = OFPhn#;
          DOOfFara = OFFara;
          DOOfFax# = OFFax#;

          DOOfAcnm = OFAcnm;
          DOOfAad1 = OFAad1;
          DOOfAad2 = OFAad2;
          DOOfActy = OFActy;
          DOOfAst  = OFAst;
          DOOfAzip = OFAzip;
          DOOfAara = OFAara;
          DOOfAphn = OFAphn;
          DOOfAfxa = OFAfxa;
          DOOfAfx# = OFAfx#;

          DOOfCknm = OFCknm;
          DOOfEftv = OFEftv;
          DOOfLnnm = OFLnnm;
          DOOfPrnm = OFPrnm;
          DOOfMinm = OFMinm;

          DOOfCnam = OFCnam;
          DOOfCad1 = OFCad1;
          DOOfCad2 = OFCad2;
          DOOfCcty = OFCcty;
          DOOfCst  = OFCst;
          DOOfCzp  = OFCzp;
          DOOfCara = OFCara;
          DOOfCphn = %int(%editc(OFCph1:'X') + %editc(OFCph2:'X'));
       //*** Beg Add *****  01/16/24  *** 153WAD ************************
          DOOfMail = OFEmail;
       //*** Beg Add *****  01/16/24  *** 153WAD ************************
       Endif;
       //*** end add *****  01/07/16  ***********************************

       //*** Beg Add *****  07/03/25  ***********************************
          DOInMail = EMmail;
       //*** End Add *****  07/03/25  ***********************************

      /end-free
     C                   Endsr                                                  Employer subr
     C**************************************************************************************
     C*
     C* Policy Year
     C*
     C**************************************************************************************
     C     PolicyYear    Begsr
      /free

       DOELPfdt = ELPfdt;
       DOELPtdt = ELPtdt;
       DOELAnpr = ELAnpr;
       DOELPror = ELPror;

       Chain keyWMRPL prp00;
       If %found;
          DOCap# = RPCap#;
       Endif;

       DOFil# = %editc(keyFnd:'X') + %editc(keyEmp#:'X') + %editc(keyFyr:'X');

      /end-free
     C                   Endsr                                                  PolicyYear subr
     C**************************************************************************************
     C*
     C* Agent
     C*
     C**************************************************************************************
     C     Agent         Begsr
      /free
       //*** begin add ***  09/27/16  **************************************
       Chain (keyCo#:keyFnd) pex00;
       If %found and EXAHdr <> ' ';
          DOAgtHdg = %trim(EXAHdr);
       Else;
          DOAgtHdg = 'Agent#';
       Endif;
       //*** end add *****  09/27/16  **************************************

       Setgt keyWMAHL pah00;
       Readpe key1WMAHL pah00;
       //*** begin add ***  10/31/17  **************************************
       Dow not %eof and AHSTAT = 'I';
          Readpe key1WMAHL pah00;
       Enddo;
       //*** end add *****  10/31/17  **************************************

       If not %eof;

       //* Primary Agency
          xxAgt# = AHAgt#;
          xxProd = AHProd;
          Chain keyWMAAL1 paa01;
          If %found and xxAgt# <> 0;
             xxFein = AAFein;
             xxMod  = AAMod;
             Chain keyWMAGL pag00;
             If %found;
                DOA1Agt# = xxAgt#;
                DOA1Efdt = AHEfdt;
                DOA1Fein = xxFein;
                DOA1Mod  = xxMod;
                DOA1Name = AGName;
                DOA1Mad1 = AGMad1;
                DOA1MCty = AGMCty;
                DOA1MSt  = AGMSt;
                DOA1MZip = AGMZip;
                DOA1Pad1 = AGPad1;
                DOA1PCty = AGPCty;
                DOA1PSt  = AGPSt;
                DOA1PZip = AGPZip;
                DOA1Are1 = AGAre1;
                DOA1Phn1 = AGPhn1;
                DOA1Are2 = AGAre2;
                DOA1Phn2 = AGPhn2;
                DOA1Faxa = AGFaxa;
                DOA1Fax# = AGFax#;
                DOA1Emal = AGEmal;
                DOA1Secy = AGSecy;
                DOA1Cont = AGCont;
                DOA1Comm = AGComm;
                DOA1Com2 = AGCom2;
                Chain keyWMAPL pap00;
                If %found and xxProd <> 0;
                   DOA1Prod = xxProd;
                   DOA1PLsn = APLsn;
                   DOA1PFsn = APFsn;
                   DOA1PEml = APEmal;
                Endif;

             Endif;

          Endif;

       //* Primary Agency of Record
          xxAgt# = AHAgrc;
          xxProd = AHPrrc;
          Chain keyWMAAL1 paa01;
          If %found and xxAgt# <> 0;
             xxFein = AAFein;
             xxMod  = AAMod;
             Chain keyWMAGL pag00;
             If %found;
                DOR1Agt# = xxAgt#;
                DOR1Efdt = AHEfdt;
                DOR1Fein = xxFein;
                DOR1Mod  = xxMod;
                DOR1Name = AGName;
                DOR1Mad1 = AGMad1;
                DOR1MCty = AGMCty;
                DOR1MSt  = AGMSt;
                DOR1MZip = AGMZip;
                DOR1Pad1 = AGPad1;
                DOR1PCty = AGPCty;
                DOR1PSt  = AGPSt;
                DOR1PZip = AGPZip;
                DOR1Are1 = AGAre1;
                DOR1Phn1 = AGPhn1;
                DOR1Are2 = AGAre2;
                DOR1Phn2 = AGPhn2;
                DOR1Faxa = AGFaxa;
                DOR1Fax# = AGFax#;
                DOR1Emal = AGEmal;
                DOR1Secy = AGSecy;
                DOR1Cont = AGCont;
                DOR1Comm = AGComm;
                DOR1Com2 = AGCom2;
                Chain keyWMAPL pap00;
                If %found and xxProd <> 0;
                   DOR1Prod = xxProd;
                   DOR1PLsn = APLsn;
                   DOR1PFsn = APFsn;
                   DOR1PEml = APEmal;
                Endif;

             Endif;

          Endif;

       //* Secondary Agency
          xxAgt# = AHSAgt;
          xxProd = AHSPrd;
          Chain keyWMAAL1 paa01;
          If %found and xxAgt# <> 0;
             xxFein = AAFein;
             xxMod  = AAMod;
             Chain keyWMAGL pag00;
             If %found;
                DOA2Agt# = xxAgt#;
                DOA2Efdt = AHEfdt;
                DOA2Fein = xxFein;
                DOA2Mod  = xxMod;
                DOA2Name = AGName;
                DOA2Mad1 = AGMad1;
                DOA2MCty = AGMCty;
                DOA2MSt  = AGMSt;
                DOA2MZip = AGMZip;
                DOA2Pad1 = AGPad1;
                DOA2PCty = AGPCty;
                DOA2PSt  = AGPSt;
                DOA2PZip = AGPZip;
                DOA2Are1 = AGAre1;
                DOA2Phn1 = AGPhn1;
                DOA2Are2 = AGAre2;
                DOA2Phn2 = AGPhn2;
                DOA2Faxa = AGFaxa;
                DOA2Fax# = AGFax#;
                DOA2Emal = AGEmal;
                DOA2Secy = AGSecy;
                DOA2Cont = AGCont;
                DOA2Comm = AGComm;
                DOA2Com2 = AGCom2;
                Chain keyWMAPL pap00;
                If %found and xxProd <> 0;
                   DOA2Prod = xxProd;
                   DOA2PLsn = APLsn;
                   DOA2PFsn = APFsn;
                   DOA2PEml = APEmal;
                Endif;

             Endif;

          Endif;

       //* Secondary Agency of Record
          xxAgt# = AH2Arc;
          xxProd = AH2Prc;
          Chain keyWMAAL1 paa01;
          If %found and xxAgt# <> 0;
             xxFein = AAFein;
             xxMod  = AAMod;
             Chain keyWMAGL pag00;
             If %found;
                DOR2Agt# = xxAgt#;
                DOR2Efdt = AHEfdt;
                DOR2Fein = xxFein;
                DOR2Mod  = xxMod;
                DOR2Name = AGName;
                DOR2Mad1 = AGMad1;
                DOR2MCty = AGMCty;
                DOR2MSt  = AGMSt;
                DOR2MZip = AGMZip;
                DOR2Pad1 = AGPad1;
                DOR2PCty = AGPCty;
                DOR2PSt  = AGPSt;
                DOR2PZip = AGPZip;
                DOR2Are1 = AGAre1;
                DOR2Phn1 = AGPhn1;
                DOR2Are2 = AGAre2;
                DOR2Phn2 = AGPhn2;
                DOR2Faxa = AGFaxa;
                DOR2Fax# = AGFax#;
                DOR2Emal = AGEmal;
                DOR2Secy = AGSecy;
                DOR2Cont = AGCont;
                DOR2Comm = AGComm;
                DOR2Com2 = AGCom2;
                Chain keyWMAPL pap00;
                If %found and xxProd <> 0;
                   DOR2Prod = xxProd;
                   DOR2PLsn = APLsn;
                   DOR2PFsn = APFsn;
                   DOR2PEml = APEmal;
                Endif;

             Endif;

          Endif;

       //* Tertiary Agency
          xxAgt# = AH3Agt;
          xxProd = AH3Prd;
          Chain keyWMAAL1 paa01;
          If %found and xxAgt# <> 0;
             xxFein = AAFein;
             xxMod  = AAMod;
             Chain keyWMAGL pag00;
             If %found;
                DOA3Agt# = xxAgt#;
                DOA3Efdt = AHEfdt;
                DOA3Fein = xxFein;
                DOA3Mod  = xxMod;
                DOA3Name = AGName;
                DOA3Mad1 = AGMad1;
                DOA3MCty = AGMCty;
                DOA3MSt  = AGMSt;
                DOA3MZip = AGMZip;
                DOA3Pad1 = AGPad1;
                DOA3PCty = AGPCty;
                DOA3PSt  = AGPSt;
                DOA3PZip = AGPZip;
                DOA3Are1 = AGAre1;
                DOA3Phn1 = AGPhn1;
                DOA3Are2 = AGAre2;
                DOA3Phn2 = AGPhn2;
                DOA3Faxa = AGFaxa;
                DOA3Fax# = AGFax#;
                DOA3Emal = AGEmal;
                DOA3Secy = AGSecy;
                DOA3Cont = AGCont;
                DOA3Comm = AGComm;
                DOA3Com2 = AGCom2;
                Chain keyWMAPL pap00;
                If %found and xxProd <> 0;
                   DOA3Prod = xxProd;
                   DOA3PLsn = APLsn;
                   DOA3PFsn = APFsn;
                   DOA3PEml = APEmal;
                Endif;

             Endif;

          Endif;

       //* Tertiary Agency of Record
          xxAgt# = AH3Arc;
          xxProd = AH3Prc;
          Chain keyWMAAL1 paa01;
          If %found and xxAgt# <> 0;
             xxFein = AAFein;
             xxMod  = AAMod;
             Chain keyWMAGL pag00;
             If %found;
                DOR3Agt# = xxAgt#;
                DOR3Efdt = AHEfdt;
                DOR3Fein = xxFein;
                DOR3Mod  = xxMod;
                DOR3Name = AGName;
                DOR3Mad1 = AGMad1;
                DOR3MCty = AGMCty;
                DOR3MSt  = AGMSt;
                DOR3MZip = AGMZip;
                DOR3Pad1 = AGPad1;
                DOR3PCty = AGPCty;
                DOR3PSt  = AGPSt;
                DOR3PZip = AGPZip;
                DOR3Are1 = AGAre1;
                DOR3Phn1 = AGPhn1;
                DOR3Are2 = AGAre2;
                DOR3Phn2 = AGPhn2;
                DOR3Faxa = AGFaxa;
                DOR3Fax# = AGFax#;
                DOR3Emal = AGEmal;
                DOR3Secy = AGSecy;
                DOR3Cont = AGCont;
                DOR3Comm = AGComm;
                DOR3Com2 = AGCom2;
                Chain keyWMAPL pap00;
                If %found and xxProd <> 0;
                   DOR3Prod = xxProd;
                   DOR3PLsn = APLsn;
                   DOR3PFsn = APFsn;
                   DOR3PEml = APEmal;
                Endif;

             Endif;

          Endif;

       Endif;

      /end-free
     C                   Endsr                                                  Agent subr
     C**************************************************************************************
     C*
     C* Contacts
     C*
     C**************************************************************************************
     C     Contacts      Begsr
      /free
       //*** begin add ***  12/15/15  **************************************
       xxCount = 0;
       //*** end add *****  12/15/15  **************************************

       Setll keyWMECL pec00;
       Reade keyWMECL pec00;

       Dow not %eof;

          Clear pdo0p1;

          //* Key fields
       //*** begin chg ***  08/04/16  ******************************************
          //*****DCXml  = xmlFile;
          DCXml  = XPXMLT;
       //*** end chg *****  08/04/16  ******************************************
          DCCo#  = keyCo#;
          DCFnd  = keyFnd;
          DCEmp# = keyEmp#;
          DCDiv  = keyDiv;
          DCFyr  = keyFyr;
          DCCase = keyCase;

          //* Contact data
          DCEcPcnt = ECPcnt;
          DCEcPosn = ECPosn;
          DCEcTitl = ECTitl;
          DCEcFsn  = ECFsn;
          DCEcMid  = ECMid;
          DCEcLsn  = ECLsn;
          DCEcAdr1 = ECAdr1;
          DCEcAdr2 = ECAdr2;
          DCEcCty  = ECCty;
          DCEcSt   = ECSt;
          DCEcZip  = ECZip;
          DCEcArea = ECArea;
          DCEcPhn# = ECPhn#;
          DCEcExt# = ECExt#;
          DCEcFara = ECFara;
          DCEcFPhn = ECFPhn;
          DCEcCara = ECCara;
          DCEcCPhn = ECCPhn;
          DCEcPphf = ECPphf;
          DCEcEmal = ECEmal;

          //* Maintenance info.
          DCPgm  = Q1PGM;
          DCUser = Q1USER;
          DCTime = Timeto6(SystemTime());
          DCDt   = Dateto7(SystemDate());
          //*** begin add ***  12/15/15  **************************************
          xxCount = xxCount +1;
          DCKey = xxCount;
          //*** end add *****  12/15/15  **************************************

          //* Write contact workfile record.
          Write pdo0p1;

          Reade keyWMECL pec00;
       Enddo;

      /end-free
     C                   Endsr                                                  Contacts subr
     C**************************************************************************************
     C*
     C* Claim
     C*
     C**************************************************************************************
     C     Claim         Begsr
      /free

       DOClaim# = %editc(keyFnd:'X') + '-' + %editc(keyFyr:'X') + '-' +
        %editc(keyCase:'X');
       DOCmACL# = CMACL#;
       DOCmECS# = CMECS#;
       DOCmRECL = CMRECL;
      //*********  Begin Chg  ***  06/09/20  **********************
      //***** DOCmSS#  = CMSS#;
       DOCmSS#= MaskData('SSN':'CMSS#':CMSS#);
      //*********  End   Chg  ***  06/09/20  **********************
       //*** begin add ***  11/22/16  ****************************************************
       DOCmRcvd = CMRcvd;
       //*** end add *****  11/22/16  ****************************************************
       //*** begin add ***  03/26/18  ***********************************   **
       DOOnEmpPr = CMeadr;
       //*** end add *****  03/26/18  ***********************************   **

       //* Claim Demographics
       Chain CMSS# pcd00;
       If %found;

          DOCdLsn  = CDLsn;
          DOCdFsn  = CDFsn;
          DOCdMid  = CDMid;
          DOCdAdr1 = CDAdr1;
          DOCdAdr2 = CDAdr2;
          DOCdAdr3 = CDAdr3;
          DOCdCty  = CDCty;
          DOCdSt   = CDSt;
          DOCdZip  = CDZip;

          //* Pull County description.
          xxKey = %Trim(CDSt) + %Trim(CDCnty);
          Chain (keyCo#:xxKey) pcn01;
          If %found;
             DOCdCnty = %trim(CnDesc);
          Endif;

          //* Pull Country Description.
          xxTbid = 'CTRY';
          xxkey = CDCtry;
          Chain keyWTGTL pgt00;
          If %found;
             DOCDCtry = %trim(GTDesc);
          Else;
             DOCDCtry = %trim(CDCtry);
          Endif;

          DOCdArea = CDArea;
          DOCdPhn# = CDPhn#;
          DOCdWAra = CDWAra;
          DOCdWPh# = CDWPh#;
          DOCdMSts = CDMSts;
          DOCdDtoB = CDDtoB;
          //*** begin add ***  03/26/18  *************************************
          DOClmntEm = CDEmAd;
          DOClmntSx = CDSex;
          //*** end add *****  03/26/18  *************************************

       Endif;

       //* Set Claim status
       If CMStat = 'O';
          Setll keyWMCHL5 pch05;
          Reade key1WMCHL5 pch05;
          Dow not %eof;
             If CHOcst = 'C';
                CMStat = 'R';
                Leave;
             Endif;
             Reade key1WMCHL5 pch05;
          Enddo;
       Endif;

       Select;
       When CMStat = 'O';
          DOCmStat = 'Open';
       When CMStat = 'C';
          DOCmStat = 'Closed';
       When CMStat = 'R';
          DOCmStat = 'Reopened';
       Endsl;

       DOCmAcdt = CMAcdt;
       DOCmDtrp = CMDtrp;
       DOCmAcds = CMAcds;
       DOCmAcd2 = CMAcd2;
       //*** begin add ***  04/03/18  ***************************************
       DOCmActm = CmActm;
       //*** end   add ***  04/03/18  ***************************************
       //*** begin add ***  04/20/16  ***************************************
       //*****DOCmLtfl = CMLtfl;
       Select;
       When CmLtfl = 'I';
          DOCmLtfl = 'Indemnity';
       When CMLtfl = 'M';
          DOCMLtfl = 'Medical';
       When CMLtfl = 'R';
          DOCmLtfl = 'Record Only';
       Endsl;
       //*** end add *****  04/20/16  ***************************************
       DOCmClas = CMClas;
       DOCmSpec = CMSpec;

       Clear outDate;

       SrEffD(%editc(keyCo#:'X'):%editc(keyFnd:'X'):%editc(keyFyr:'X'):
        %editc(keyEmp#:'X'):%editc(keyDiv:'X'):outDate:'P':%editc(CMClas:'X'));

       xxEffDate = %int(outDate);

       Setgt keyWMPRL1 ppr01;
       Readpe key1WMPRL1 ppr01;
       If not %eof;
          DOCmClasd = %trim(PRDesc);
       Endif;

       DOCmAww  = CMAww;
       //*** begin add ***  11/27/17  **************************************
       Setll (keyCo#:keyFnd:keyFyr:keyCase) paw01;
       Reade(n) (keyCo#:keyFnd:keyFyr:keyCase) paw01;
       If not %eof;
          DOPreAWW = AWAWW;
          //*** Begin Add *** 06/11/18 *************************************
          DOPreCmp = AWcrt;
          //*** End   Add *** 06/11/18 *************************************
       Endif;
       //*** end add *****  11/27/17  **************************************
       DOCmDtlw = CMDtlw;
       DOCmRlw  = CMRlw;
       DOCmEspv = CMEspv;
       DOCmOcds = CMOcds;
       DOCmOccd = CMOccd;

       //* Occupation Code Description
       xxKey = CMOccd;
       Chain (keyCo#:xxKey) poc00;
       If %found;
          DOCmOccdD = %trim(OCDesc);
       Endif;

       //* Department Description
       Chain keyWMDPL pdp00;
       If %found;
          DOCmDept = %trim(DPDesc);
       Endif;

       DOCmHidt = CMHidt;

       //* Employment Status
       xxTbid = 'EMST';
       xxkey = CMEsts;
       Chain keyWTGTL pgt00;
       If %found;
          DOCMEsts = %trim(GTDesc);
       Endif;

       //* Shift
       xxTbid = 'SHFT';
       xxkey = CMShft;
       Chain keyWTGTL pgt00;
       If %found;
          DOCMShft = %trim(GTDesc);
       Endif;

       DOCmDays = CMDays;
       DOCmHrsD = CMHrsD;

       //*** begin add ***  01/07/16  *************************************
       //* Nature
       DOCmNatr = CMNatr;
       xxKey = CMNatr;
       Chain keyWTABL pna01;
       If %found;
          DOCmNatrD = %trim(NADesc);
       Endif;

       //* Part
       DOCmPart = CMPart;
       xxKey = CMPart;
       Chain keyWTABL ppa01;
       If %found;
          DOCmPartD = %trim(PADesc);
       Endif;

       //* Cause
       DOCmCaus = CMCaus;
       xxKey = CMCaus;
       Chain keyWTABL pca01;
       If %found;
          DOCmCausD = %trim(CADesc);
       Endif;

       //* Jurisdiction state
       DOCmJrst = CMJrst;
       xxKey = CMJrst;
       Chain xxKey pst01;
       If %found;
          DOCmJrstD = %trim(STName);
       Endif;
       //*** end add *****  01/07/16  *************************************

       //* Claim Manager
       If CMAMan <> ' ';
          Chain(n) CMAMan pup01;
          If %found;
             DOCmName = UPUsnm;
             DOCmTitl = UPTitl;
             DOCmExt# = UPExt#;
             DOCmMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOCmPhnA = UPPhnA;
             DOCmPhn# = UPPhn#;
             DOCmFaxA = UPFaxA;
             DOCmFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Case Nurse
       If CMNrse <> ' ';
          Chain(n) CMNrse pup01;
          If %found;
             DOCnName = UPUsnm;
             DOCnTitl = UPTitl;
             DOCnExt# = UPExt#;
             DOCnMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOCnPhnA = UPPhnA;
             DOCnPhn# = UPPhn#;
             DOCnFaxA = UPFaxA;
             DOCnFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Reinsurance/Excess Adjuster
       If CMESup <> ' ';
          Chain(n) CMEsup pup01;
          If %found;
             DOEaName = UPUsnm;
             DOEaTitl = UPTitl;
             DOEaExt# = UPExt#;
             DOEaMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOEaPhnA = UPPhnA;
             DOEaPhn# = UPPhn#;
             DOEaFaxA = UPFaxA;
             DOEaFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* SDF Adjuster
       If CMSDFA <> ' ';
          Chain(n) CMSDFA pup01;
          If %found;
             DOSaName = UPUsnm;
             DOSaTitl = UPTitl;
             DOSaExt# = UPExt#;
             DOSaMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOSaPhnA = UPPhnA;
             DOSaPhn# = UPPhn#;
             DOSaFaxA = UPFaxA;
             DOSaFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Supervisor
       If CMSupr <> ' ';
          Chain(n) CMSupr pup01;
          If %found;
             DOSpName = UPUsnm;
             DOSpTitl = UPTitl;
             DOSpExt# = UPExt#;
             DOSpMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOSpPhnA = UPPhnA;
             DOSpPhn# = UPPhn#;
             DOSpFaxA = UPFaxA;
             DOSpFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Claims Examiner
       If CMClex <> ' ';
          Chain(n) CMClex pup01;
          If %found;
             DOCeName = UPUsnm;
             DOCeTitl = UPTitl;
             DOCeExt# = UPExt#;
             DOCeMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOCePhnA = UPPhnA;
             DOCePhn# = UPPhn#;
             DOCeFaxA = UPFaxA;
             DOCeFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
             //*** begin add ***  03/26/18  *************************************
             Chain(n) CMClex pup02;
             If %found;
                eval DOAltNuma = ##UPAltA;
                eval DOAltNump = ##UPAltP;
             Endif;
             //*** end add *****  03/26/18  *************************************
          Endif;
       Endif;

       //* Unit Manager
       If CMUMan <> ' ';
          Chain(n) CMUMan pup01;
          If %found;
             DOUmName = UPUsnm;
             DOUmTitl = UPTitl;
             DOUmExt# = UPExt#;
             DOUmMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOUmPhnA = UPPhnA;
             DOUmPhn# = UPPhn#;
             DOUmFaxA = UPFaxA;
             DOUmFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Field Adjuster
       If CMFead <> ' ';
          Chain(n) CMFead pup01;
          If %found;
             DOFaName = UPUsnm;
             DOFaTitl = UPTitl;
             DOFaExt# = UPExt#;
             DOFaMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOFaPhnA = UPPhnA;
             DOFaPhn# = UPPhn#;
             DOFaFaxA = UPFaxA;
             DOFaFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       //* Claim Enter Clerk
       If CMCec <> ' ';
          Chain(n) CMCec pup01;
          If %found;
             DOCbName = UPUsnm;
             DOCbTitl = UPTitl;
             DOCbExt# = UPExt#;
             DOCbMail = UPMail;
             //*** begin add ***  08/06/15  *************************************
             DOCbPhnA = UPPhnA;
             DOCbPhn# = UPPhn#;
             DOCbFaxA = UPFaxA;
             DOCbFax# = UPFax#;
             //*** end add *****  08/06/15  *************************************
          Endif;
       Endif;

       DOCmDny  = CMDny;
       DOCmCvfl = CMCvfl;
       //*** begin chg ***  04/29/16  ***************************************
       //*****DOCmSubr = CMSubr;
       Select;
       When CmSubr = 'N';
          DOCmSubr = 'Not Subrogated';
       When CMSubr = 'P';
          DOCMSubr = 'Pending Investigation';
       When CMSubr = 'R';
          DOCmSubr = 'Subrogation Recovery';
       When CMSubr = 'Y';
          DOCmSubr = 'Subrograted';
       Endsl;
       //*** end chg *****  04/29/16  ***************************************
       DOCmAcqr = CMAcqr;
       DOCmSDF  = CMSDF;
       DOCmPlfl = CMPlfl;
       DOCmPoi  = CMPoi;
       DOCmExcs = CMExcs;

       //*** begin add ***  04/29/16  ***************************************
       //* Place of accident
       DOCmAcnm = %trim(CMAcnm);
       DOCmAcdr = %trim(CMAcdr);
       DOCmActy = %trim(CMActy);
       DOCmAst  = %trim(CMAst);
       If CMAcz1 <> 0;
          DOCmAzip = (CMAcz1 * 10000) + CMAcz2;
       Endif;

       xxKey = %Trim(CMAst) + %Trim(CMAccy);

       Chain (keyCo#:xxKey) pcn01;

       If %found;
          DOCMAccy = %trim(CnDesc);
       Endif;
       //*** end add *****  04/29/16  ***************************************

       //* Claim Medical
       DOCmMco  = CMMco;
       DOCmMctp = CMMctp;

       //* MCO Type description
       xxTbid = 'MCO';
       xxkey = CMMctp;
       Chain keyWTGTL pgt00;
       If %found;
          DOCmMctpd = %trim(GTDesc);
       Endif;

       //* Claim Physician
       Setll keyWMTPL1 ptp01;
       Reade key1WMTPL1 ptp01;
       Dow not %eof;
          If TPPRol = 'TR' and (TPTodt >= curDate or TPTodt = 0);
             DOTpPhyF = TPPhyF;
             DOTpPhyM = TPPhyM;
             DOTpPhyN = TPPhyN;
             DOTpPhyA = TPPhyA;
             DOTpPhyC = TPPhyC;
             DOTpPhyS = TPPhyS;
             DOTpPhyZ = TPPhyZ;
             DOTpPhyR = TPPhyR;
             DOTpPhyP = TPPhyP;
             DOTpFxAr = TPFxAr;
             DOTpFax# = TPFax#;
             Leave;
          Endif;
          Reade key1WMTPL1 ptp01;
       Enddo;

       //* Hospital
       DOCmHosF = CMHosF;
       DOCmHosM = CMHosM;
       DOCmHosN = CMHosN;
       DOCmHosA = CMHosA;
       DOCmHosC = CMHosC;
       DOCmHosS = CMHosS;
       DOCmHosZ = CMHosZ;
       DOCmHosR = CMHosR;
       DOCmHosP = CMHosP;

       DOCmItrt = CMItrt;

       //* Initial Treatment description
       xxTbid = 'INIT';
       xxkey = CMItrt;
       Chain keyWTGTL pgt00;
       If %found;
          DOCmItrtD = %trim(GTDesc);
       Endif;

       DOCmSgdt = CMSgdt;
       DOCmMMI  = CMMMI;
       DOCmIpct = CMIpct;
       DOCmYrlx = CMYrlx;
       DOCmDod  = CMDod;
       DOCmOdis = CMOdis;
       DOCmHcap = CMHcap;
       DOCmDth  = CMDth;
       DOCmImp  = CMImp;

       //* Claim Benefits
       DOCmFsal = CMFsal;
       DOCmFsdt = CMFsdt;
       DOCmPrww = CMPrww;
       DOCmWmth = CMWmth;

       //* Method of Pre Injury Wages description
       xxTbid = 'WMTH';
       xxkey = CMWmth;
       Chain keyWTGTL pgt00;
       If %found;
          DOCmWmthD = %trim(GTDesc);
       Endif;

       DOCmPoww = CMPoww;
       DOCmCrt  = CMCrt;

       //* Wage Period description
       xxTbid = 'FRWP';
       xxkey = CMWgpd;
       Chain keyWTGTL pgt00;
       If %found;
          DOCmWgpdD = %trim(GTDesc);
       Endif;

       DOCmDbdt = CMDbdt;
       DOCmErtw = CMErtw;
       DOCmRtw  = CMRtw;

       //* Claim Legal

       //* Legal Master
       Chain keyWMLLL pll00;
       If %found;

          //* Defense Attorney
          DOLlDefa = LLDefa;
          DOLlDefm = LLDefm;
          DOLlDefn = LLDefn;
          //*** begin add ***  04/08/16  *************************************
          If LLDefn = ' ';
             kkFein = LLDefA;
             kkMod  = LLDefM;
             Chain(n) keyWMPDL ppd00;
             If %found(WMPDL);
                DOLlDefn = %trim(PDFsn) + ' ' + %trim(PDName);
             Endif;
          Endif;
          //*** end add *****  04/08/16  *************************************
          DOLlDad1 = LLDad1;
          DOLlDad2 = LLDad2;
          DOLlDCty = LLDCty;
          DOLlDSt  = LLDSt;
          DOLlDZip = LLDZip;
          DOLlDEfr = LLDEfr;
          DOLlDEfp = LLDEfp;
          DOLlDEfe = LLDEfe;
          DOLlDEff = LLDEff;
          DOLlDEdt = LLDEdt;
          DOLlEmdd = %trim(LLEmdd);

          //* Plantiff Attorney
          DOLlPlaa = LLPlaa;
          DOLlPlam = LLPlam;
          DOLlPlan = LLPlan;
          //*** begin add ***  04/08/16  *************************************
          If LLPlan = ' ';
             kkFein = LLPlaA;
             kkMod  = LLPlaM;
             Chain(n) keyWMPDL ppd00;
             If %found(WMPDL);
                DOLlPlan = %trim(PDFsn) + ' ' + %trim(PDName);
             Endif;
          Endif;
          //*** end add *****  04/08/16  *************************************
          DOLlPad1 = LLPad1;
          DOLlPad2 = LLPad2;
          DOLlPCty = LLPCty;
          DOLlPSt  = LLPSt;
          DOLlPZip = LLPZip;
          DOLlPlar = LLPlar;
          DOLlPlap = LLPlap;
          DOLlPlae = LLPlae;
          DOLlPlaf = LLPlaf;
          DOLlDtap = LLDtap;
          DOLlEmdp = %trim(LLEmdp);
          DOLlClri = LLClri;

          //* Third Party Attorney
          DOLlTpaa = LLTpaa;
          DOLlTpam = LLTpam;
          DOLlTpan = LLTpan;
          //*** begin add ***  04/08/16  *************************************
          If LLTpan = ' ';
             kkFein = LLTpaa;
             kkMod  = LLTpam;
             Chain(n) keyWMPDL ppd00;
             If %found(WMPDL);
                DOLlTpan = %trim(PDFsn) + ' ' + %trim(PDName);
             Endif;
          Endif;
          //*** end add *****  04/08/16  *************************************
          DOLlTad1 = LLTad1;
          DOLlTad2 = LLTad2;
          DOLlTCty = LLTCty;
          DOLlTSt  = LLTSt;
          DOLlTZip = LLTZip;
          DOLlTpar = LLTpar;
          DOLlTpap = LLTpap;
          DOLlTpae = LLTpae;
          DOLlTpaf = LLTpaf;
          DOLlTpdt = LLTpdt;
          DOLlEmdt = %trim(LLEmdt);

          //* Legal indicators
          DOLlFrau = LLFrau;

          //* Fraud indicator description
          xxTbid = 'FRAU';
          xxkey = LLFrau;
          Chain keyWTGTL pgt00;
          If %found;
             DOLlFrauD = %trim(GTDesc);
          Endif;

          DOLlLitg = LLLitg;
          DOLlLump = LLLump;

          //* Lawsuit/Settlement info
          DOLlLwdt = LLLwdt;
          DOLlMtst = LLMtst;

          //* Settlement method description
          xxTbid = 'LSST';
          xxkey = %trim(CMJrst) + %trim(LLMtst);
          Chain keyWTGTL pgt00;
          If %found;
             DOLlMtstD = %trim(GTDesc);
          Endif;

          DOLlStdt = LLStdt;
          DOLlSamt = LLSamt;
          DOLlIscm = LLIscm;
          DOLlMscm = LLMscm;
          DOLlFdly = LLFdly;

          //*** begin add ***  07/31/15  *************************************
          DOLLWcab = LLWcab;
          //*** end add *****  07/31/15  *************************************

       Endif;

       //* Claim Reserves

       //* Claim Reserves payment against
       DOCmAr01 = CMAr01;
       DOCmAr02 = CMAr02;
       DOCmAr03 = CMAr03;
       DOCmAr04 = CMAr04;
       DOCmAr05 = CMAr05;
       DOCmAr06 = CMAr06;
       DOCmAr07 = CMAr07;
       DOCmAr08 = CMAr08;
       DOCmAr09 = CMAr09;
       DOCmAr10 = CMAr10;

       //* Claim Reserves remaining
       DOCmRr01 = CMRr01;
       DOCmRr02 = CMRr02;
       DOCmRr03 = CMRr03;
       DOCmRr04 = CMRr04;
       DOCmRr05 = CMRr05;
       DOCmRr06 = CMRr06;
       DOCmRr07 = CMRr07;
       DOCmRr08 = CMRr08;
       DOCmRr09 = CMRr09;
       DOCmRr10 = CMRr10;

       //* Claim Reserves total incurred
       DOCmTI01 = CMRr01 + CMAr01;
       DOCmTI02 = CMRr02 + CMAr02;
       DOCmTI03 = CMRr03 + CMAr03;
       DOCmTI04 = CMRr04 + CMAr04;
       DOCmTI05 = CMRr05 + CMAr05;
       DOCmTI06 = CMRr06 + CMAr06;
       DOCmTI07 = CMRr07 + CMAr07;
       DOCmTI08 = CMRr08 + CMAr08;
       DOCmTI09 = CMRr09 + CMAr09;
       DOCmTI10 = CMRr10 + CMAr10;

       //* Claim Reserves 01
       If DOCmTI01 <> 0;
          xxKey = CMRT01;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs01 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy01 = xxResType;
          Endif;
       Endif;

       //* Claim Reserves 02
       If DOCmTI02 <> 0;
          xxKey = CMRT02;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs02 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy02 = xxResType;
          Endif;
       Endif;

       //* Claim Reserves 03
       If DOCmTI03 <> 0;
          xxKey = CMRT03;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs03 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy03 = xxResType;
          Endif;
       Endif;

       //* Claim Reserves 04
       If DOCmTI04 <> 0;
          xxKey = CMRT04;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs04 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy04 = xxResType;
          Endif;
       Endif;

       //* Claim Reserves 05
       If DOCmTI05 <> 0;
          xxKey = CMRT05;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs05 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy05 = xxResType;
          Endif;
       Endif;

       //* Claim Reserves 06
       If DOCmTI06 <> 0;
          xxKey = CMRT06;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs06 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy06 = xxResType;
          Endif;
       Endif;

       //* Claim Reserves 07
       If DOCmTI07 <> 0;
          xxKey = CMRT07;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs07 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy07 = xxResType;
          Endif;
       Endif;

       //* Claim Reserves 08
       If DOCmTI08 <> 0;
          xxKey = CMRT08;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs08 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy08 = xxResType;
          Endif;
       Endif;

       //* Claim Reserves 09
       If DOCmTI09 <> 0;
          xxKey = CMRT09;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs09 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy09 = xxResType;
          Endif;
       Endif;

       //* Claim Reserves 10
       If DOCmTI10 <> 0;
          xxKey = CMRT10;
          Chain keyWTABL prt01;
          If %found;
             DOCmDs10 = %trim(RTDesc);
             Exsr ResType;
             DOCmTy10 = xxResType;
          Endif;
       Endif;

       //*** begin add ***  12/18/15  **************************************
       //* Claim Category 2

       Chain keyWMCXL pcx00;
       If %found and CXCat2 <> ' ';
          DOCat2Hd = CXCat2;
       Else;
          DOCat2Hd = 'Category 2';
       Endif;

       DOCMCtg2 = CMCtg2;

       //*** end add *****  12/18/15  **************************************

       //*** begin add ***  01/07/16  ***********************************
       //* Claim Office
       Chain (keyCo#:CMOff#) pof00;
       If %found;

          DOOcName = OFName;
          DOOcAdr1 = OFAdr1;
          DOOcAdr2 = OFAdr2;
          DOOcCty  = OFCty;
          DOOcSt   = OFSt;
          DOOcZip  = OFZip;
          DOOcArea = OFArea;
          DOOcPhn# = OFPhn#;
          DOOcFara = OFFara;
          DOOcFax# = OFFax#;

          DOOcAcnm = OFAcnm;
          DOOcAad1 = OFAad1;
          DOOcAad2 = OFAad2;
          DOOcActy = OFActy;
          DOOcAst  = OFAst;
          DOOcAzip = OFAzip;
          DOOcAara = OFAara;
          DOOcAphn = OFAphn;
          DOOcAfxa = OFAfxa;
          DOOcAfx# = OFAfx#;

          DOOcCknm = OFCknm;
          DOOcEftv = OFEftv;
          DOOcLnnm = OFLnnm;
          DOOcPrnm = OFPrnm;
          DOOcMinm = OFMinm;

          DOOcCnam = OFCnam;
          DOOcCad1 = OFCad1;
          DOOcCad2 = OFCad2;
          DOOcCcty = OFCcty;
          DOOcCst  = OFCst;
          DOOcCzp  = OFCzp;
          DOOcCara = OFCara;
          DOOcCphn = %int(%editc(OFCph1:'X') + %editc(OFCph2:'X'));

       Endif;
       //*** end add *****  01/07/16  ***********************************

      /end-free
     C                   Endsr                                                  Claim subr
     C**************************************************************************************
     C*
     C* Pull type for reserve code.
     C*
     C**************************************************************************************
     C     ResType       Begsr
      /free

       Select;
       When  RTType = 'C';
          xxResType = 'Compensation';
       When  RTType = 'E';
          xxResType = 'Expense';
       When  RTType = 'L';
          xxResType = 'Legal';
       When  RTType = 'M';
          xxResType = 'Medical';
       Other;
          xxResType = RTType;
       Endsl;

      /end-free
     C                   Endsr                                                  ResType subr
      //*** begin add ***  03/26/18  ***********************************
     C**************************************************************************************
     C*
     C* Payments data
     C*
     C**************************************************************************************
     C     Payments      Begsr
      /free
       Setll (keyCo#:keyFnd:keyFyr:keyCase) ppa00;
       Reade(n) (keyCo#:keyFnd:keyFyr:keyCase) ppa00;
       Dow not %eof;
          If PAretp = 'C';
             If PAtype = 'R' or PAtype ='S' or PAtype = 'V';
                DOCompPyd = DOCompPyd + (PAAMT * -1);
             Else;
                DOCompPyd = DOCompPyd + PAAMT;
             Endif;
          Endif;
          Reade(n) (keyCo#:keyFnd:keyFyr:keyCase) ppa00;
       Enddo;
      /end-free
     C                   Endsr                                                  ResType subr
      //*** end add *****  03/26/18  ***********************************
     C**************************************************************************************
       //*** begin add ***  01/07/16  ***********************************
     C**************************************************************************************
     C*
     C* Trial information.
     C*
     C**************************************************************************************
     C     Trials        Begsr
      /free

       xxCount = 0;

       Setll keyWMTIL pti00;
       Reade keyWMTIL pti00;

       Dow not %eof;

          Clear pdo0p2;

          //* Key fields
       //*** begin chg ***  08/04/16  ******************************************
          //*****D2Xml  = xmlFile;
          D2Xml  = XPXMLT;
       //*** end chg *****  08/04/16  ******************************************
          D2Co#  = keyCo#;
          D2Fnd  = keyFnd;
          D2Fyr  = keyFyr;
          D2Case = keyCase;

          //* Trial data
          D2TiSudt = TISudt;
          D2TiTrtp = TITrtp;
          D2TiJudg = TIJudg;
          //*** begin add ***  04/08/16  *************************************
          //* Judge name
          xxTbid = 'JUDG';
          xxkey = TIJudg;
          Chain keyWTGTL pgt00;
          If %found;
             D2TiJudg = %trim(GTDesc);
          Endif;
          //*** end add *****  04/08/16  *************************************
          D2TiIssu = TIIssu;
          D2TiCLvl = TICLvl;
          D2TiVerd = TIVerd;
          D2TiVedt = TIVedt;
          D2TiPeti = TIPeti;

          //* Maintenance info.
          D2Pgm  = Q1PGM;
          D2User = Q1USER;
          D2Time = Timeto6(SystemTime());
          D2Dt   = Dateto7(SystemDate());
          xxCount = xxCount +1;
          D2Key = xxCount;

          //* Write trial workfile record.
          Write pdo0p2;

          Reade keyWMTIL pti00;
       Enddo;

      /end-free
     C                   Endsr                                                  Trials subr
     C**************************************************************************************
       //*** end add *****  01/07/16  ***********************************
