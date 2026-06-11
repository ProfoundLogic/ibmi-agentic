     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**************************************************************************************
      /TITLE W8903R - Extract Data for Exposure file                                       *
     H**************************************************************************************
     6*  Description: This program extracts data for the exposure file WHEXP.  It does     *
     H*               this by cycling through all employers for all groups (except the     *
     H*               test groups indicated by test flag in WMFNP).                        *
     H*                                                                                    *
     H*  Created: 08/18/00   Programmer C:87    Project:WC925                              *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*
     H* 10/23/24 C:342 Support: 117057
     H*                Recompile over Whexp file changes for Exdper field
     H*
     H* 06/17/24 C:344 Project: WC2952
     H*                Remove manRt array which is no longer used.
     H*
     H* 02/08/22 C:062 Ticket: 100582(102)
     H*                Populate EXRTDT with EPCADT
     H*
     H* 05/01/20 C:062 Ticket: 88929(146)
     H*                Allow class code 12 (Pandemic code)
     H*
     H* 07/02/18 C:136 Support: 70716
     H*                Correct per capita rates
     H*
     H* 09/15/14 C:136 Support: WA099 81242
     H*                Add large deductible information to extract
     H*
     H* 02/04/14 C:180 Support: WA133 84698
     H*                         Initialize limits for exp rec.        *
     H*                                                               *
     H* 12/03/13 C:279 Support: CI114  84330                          *
     H*                Modified to pick up Class code correctly based *
     H*                on Rate Set Code.                              *
     H*                                                               *
     H* 10/16/13 C:136 Support: WA114  83779                          *
     H*                Allow population rated classes to be written   *
     H*                to the extract                                 *
     H*                                                               *
     H* 01/07/13 C:136 Support: WA146 81385                           *
     H*                Adjust logic on exp mod dates                  *
     H*                                                               *
     H* 12/20/12 C:136 Support: WA146 81322                           *
     H*                Correct rate dates for increased limits        *
     H*                Deleted logic for other exposures because      *
     H*                they have never been written to WHEXP          *
     H*                                                               *
     H* 10/05/12  C:283  Project: FQ045 80849                         *
     H*                  Increase the size of Array "pramt".          *
     H*                                                               *
     H* 11/30/12 C:136 Support: WA146 81173                           *
     H*                Correct rate effective dates on payroll        *
     H*                exposure records                               *
     H*                                                               *
     H* 02/20/12  C:190  Project: CI112                               *
     H*                  Add payroll class extension codes to FQCI    *
     H*                                                               *
     H* 01/04/11  C:191  Project: WC1856 PRISS #10                    *
     H*                  Make processing backward compatible          *
     H*                                                               *
     H* 02/07/11 C:227 Support CI133 75059                            *
     H*                Include class codes with zero payroll          *
     H*                                                               *
     H* 11/25/10 C:220 Project: WC1856                                *
     H*                Implement WA726R API to retrieve policy        *
     H*                start policy year of the group.                *
     H*                                                               *
     H* 08/12/09 C:33  Project: CI077                                 *
     H*                Expand name.                                   *
     H*                                                               *
     H* 12/11/08 C:61  SUPPORT WA139 66384                            *
     H*                Do not populate EXHAZ#.                        *
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     H*
     H* 08/12/05 C: 132 CI093 - 47984
     H*             Retrieve the Client ID and if is KRMS (093) then bypass the check
     H*             the check for a WDEHP record and always write WHEXP record
     H*
     H*  07/22/05       C: 58      Project: 135WEH
     H*        for extracted policy#, use WMRPP/carrier number:
     H*          For: client 135;
     H*                                                                                    *
     H* 11/15/04 C: 132 CI021                                                              *
     H*             Add check on WDEHP file when a policy start date in WDELL is in the    *
     H*             future. If no WDEHP record for these future strart dates, do not       *
     H*             write the record.                                                      *
     H*                                                                                    *
     H* 08/11/04 C: 132 91DLK                                                              *
     H*             Add Exclude Employers file WHXPP, Check file when                      *
     H*             Write WHEXP record, if exists in WHXPP file then                       *
     H*             do not write the record.                                               *
     H*                                                                                    *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*                                                                                    *
     H* 02/27/04 C:132 SUPPORT CI099 40909                            *
     H*                Check for certain hex characters in output     *
     H*                alpha fields and replaced if exists. Fields    *
     H*                from tables and static files are not checked   *
     H*                This would be WTLML1 & WMPXL1 in this program. *
     H*                                                               *
     H* 10/09/03  C:132  Project: CI010                                                    *
     H*                - Add input date parm (job date) to be used as output ASOF date     *
     H*                                                                                    *
     H* 09/19/03  C:087  Support: seq# 39130                                               *
     H*                - Back out change made to set adtPyrl = *off.  See revision         *
     H*                  comment and changes for 09/18/01.                                 *
     H*                                                                                    *
     H* 06/13/02  C:132  Project: WC924 Seq: 32788                                         *
     H*                - Change to blank fields after output of fields  EXDTYP, EXDCLM,    *
     H*                  EXDAGG, EXPER which are deductible fields.                        *
     H*                                                                                    *
     H* 06/04/02  C:132  Project: WC924 Seq: 32803                                         *
     H*                - Change to correct add of limts to output field EXELIM (50)        *
     H*                - Change to blank fields after output of fields  EXDTYP, EXDCLM,    *
     H*                  EXDAGG, EXPER which are deductible fields.                        *
     H*                                                                                    *
     H* 03/20/02  C:132  Project: WC924                                                    *
     H*                - Change to ##ARAT TO MATCH LENGTH OF EPARAT (7,2)                  *
     H*                                                                                    *
     H* 03/05/02  C:132  Project: WC924                                                    *
     H*                - Change to ADD WMEMP/EPSPEC to end of WHEXP/EXCLAS field           *
     H*                                                                                    *
     H* 11/13/01  C:132  Project: WC924                                                    *
     H*                - Change to only add a Rate Date if above calcualations             *
     H*                  did not create a EXRFDT and it is blank                           *
     H*                  Remove othExp from clrPyrlArr                                     *
     H*                                                                                    *
     H* 10/18/01  C:132  Project: WC924                                                    *
     H*                - Add othExp Array to clrPyrlArr subroutine to fix                  *
     H*                  Duplicate Output Records, Change InDex on ProRate Limits Prem     *
     H*                                                                                    *
     H* 10/04/01  C:132  Project: WC924                                                    *
     H*                - Add check for WMFNP/FNCICF flag                                   *
     H*                                                                                    *
     H* 09/18/01  C:132  Project: WC924                                                    *
     H*                - Add other Arrays to clrPyrlArr subroutine to fix                  *
     H*                  Duplicate Output Records                                          *
     H*                                                                                    *
     H* 08/24/01  C:132  Project: WC924                                                    *
     H*                - Modify chain to Rate Code for Payroll Description.                *
     H*                  Chain first using a blank Rate Code state, then                   *
     H*                  use State Rate Code if not found with Blank                       *
     H*                                                                                    *
     H* 08/03/01  C:61   Project: WC924                                                    *
     H*                - Use correct rate code to get Payroll Description.                 *
     H*                - always load deductible percentage                                 *
     H*                                                                                    *
     H* 07/25/01  C:35   Project: Support                                                  *
     H*                  State-reporting division was being used in several places within  *
     H*                  the "all divs" loop instead of the division being processed.      *
     H*                  Also, added code to clear record before each employer.            *
     H*                                                                                    *
     H* 03/21/01  C:61   Project WC924                                                     *
     H*                - Change left/right alignment of fields in WHEXP.                   *
     H*                                                                                    *
     H* 02/06/01  C:61   Project WC924                                                     *
     H*                - Load ClassCodeDesc EXCLDS                                         *
     H*                    Take the class in WMEPP alon with the extension                 *
     H*                    field and state to chain to payroll extension                   *
     H*                    file.   If no hit, then take the class and rate                 *
     H*                    set code along with eff date of class to get                    *
     H*                    appropriate payroll rate record in the payroll                  *
     H*                    rate file.                                                      *
     H*                                                                                    *
     H**************************************************************************************
     Fwdell     if   e           k disk                                         emp detail
     Fwdell1    if   e           k disk    Rename(pel01 : pel02) Prefix(el2:2)   "     "
     Fwdell8    if   e           k disk    Rename(pel01 : pel03) Prefix(el3:2)   "     "
     Fwmfnl     if   e           k disk                                         fund master
     Fwmeml     if   e           k disk                                         emp master
     Fwdf2l     if   e           k disk                                         fund auxilliary
     Fwmrsl     if   e           k disk                                         rate set code
     Fststl3    if   e           k disk                                         st code table
     Fwtddl2    if   e           k disk                                         ded dscnt code
      ***** BEG CHG ***** 06/13/02 ******************************************
     F*****wdddl1    if   e           k disk    Rename(pdd01 : pd201)                deducteble dtl
     F****wdddl1    if   e           k disk    Rename(pdd01 : pd201) Prefix(dd2:2)  deducteble dtl
      ***** END CHG ***** 06/13/02 ******************************************
     Fwmepl1    if   e           k disk                                         emp payrolls
      *** beg chg ***** 11/30/12 ***********************************************
     F*********** wmepl7    if   e           k disk    Rename(pep01 : pep02) Prefix(ep2:2)
     Fwmepl7e   if   e           k disk    Rename(pep01 : pep02) Prefix(ep2:2)
      *** end chg ***** 11/30/12 ***********************************************
     Fwmchl     if   e           k disk                                         claim history
     Fwtabl     if   e           k disk    Include(ppa01 : pna01 : pca01 :      table logical
     F                                                   prt01 : pbs01 )
     Fwtocl     if   e           k disk                                         occupation code
     Fwdehl     if   e           k disk                                         emp dtl history
     Fwdrrl     if   e           k disk                                         stat rptg rating
     Fwtgtl     if   e           k disk                                         generic table file
     Fwmprl1    if   e           k disk                                         payroll class
     Fwmlll     if   e           k disk                                         claim legal mstr
     Fwamcl3    if   e           k disk                                         multiple claim
     Fwtlml1    if   e           k disk                                         limits table file
     F***** BEG ADD ***** 02/06/01 *********************************************************
     Fwmpxl1    if   e           k disk                                         limits table file
     F***** END ADD ***** 02/06/01 *********************************************************
     F**** Begin Add ****  07/22/05  **************************************
     F*   Employer Carrier File
     Fwmrpl     if   e           k disk                                         limits table file
     F**** End Add ******  07/22/05  **************************************

      ***** BEG ADD ***** 08/11/04 *********************************************
      * Excluded Employers file
     FWHXPL     if   e           k disk                                         limits table file
      ***** END ADD ***** 08/11/04 *********************************************
      *** beg add ***** 09/15/14 ***********************************************
      * Large Deductible File
     fwmldl1    if   e           k disk
      *** end add ***** 09/15/14 ***********************************************
     F*
      * Data Warehouse - exposure file
     Fwhexp     uf a e             disk

      ***** BEG ADD ***** 02/27/04 ************************************
     d hx              s              1    dim(40)                              HEX CNVT ARRY
      ***** END ADD ***** 02/27/04 ************************************

     D*  Procedure prototypes
     D/copy sprdate
     D/copy wpremplr
     D
     D*** Begin Del *** 05/0/20 ********************************************
     D*** dt6to8           pr             8  0
     D***  numDate6                       6  0 const
     D*** End   Del *** 05/0/20 ********************************************
     D

      *** beg chg ***** 11/30/12 **************************************
     D*splitPyrl*      pr
     D***##co#***                     3  0 Const
     D***##fnd***                     3  0 Const
     D***##fyr***                     3  0 Const
     D***##emp#**                     9  0 Const
     D***##div***                     5  0 Const
     D***##clas**                     5  0 Const
     D***##spec**                     1    Const
     D***##idx***                     3  0 Const
     D***##eamt**                    11  2
     D***##prem**                    11  2
      ******BEG*CHG ******* 03/20/02 *********************************************
     D***##arat******                 5  2 Const
     D***##arat**                     7  2 Const
      ******END*CHG ******* 03/20/02 *********************************************
     D***##efdt**                     7  0 Const
     D***##beg_dt                     7  0 Const
     D***##end_dt                     7  0 Const
     D***##percap                     1    Const

      *** beg chg ***** 12/20/12 **************************************
     d*splitClass******pr
     d*wrtPayExp*******pr
     d**payRoll********                    like(EPEAPR)
     d**premAmt********                    like(EPEMPR)
     d**begSpltDt******               7  0
     d**endSpltDt******               7  0
     d**index**********               2  0
     d*getClasDesc*****pr                  like(EXclds)
     d splitClass      pr
     d  typeOfClass                   1    const
     d procSplit       pr
     d  typeOfClass                   1    const
     d  tableKey                     10
     d  payRollAmt                         like(EPEAPR)
     d  clsPremAmt                         like(EPEMPR)
     d  perBegDate                    7  0
     d  perEndDate                    7  0
     d wrtExposure     pr
     d  typeOfClass                   1    const
     d  tableKey                     10
     d  payRoll                            like(EPEAPR)
     d  premAmt                            like(EPEMPR)
     d  begSpltDt                     7  0
     d  endSpltDt                     7  0
     d  index                         2  0
     d getClasInfo     pr
     d  typeOfClass                   1    const
     d  tableKey                     10
     d  clasCode                     10
     d  clasDesc                    100
     d  tableKey       s             10
     d  clasCode       s             10
     d  clasDesc       s            100
      *** end chg ***** 12/20/12 **************************************
     d rmvHexChar      pr
     d  desc                               like(EXclds) options(*varsize)
      *** end chg ***** 11/30/12 **************************************


     D getDedTyp       pr             2
     D   company                      3  0
     D   dedRsvArr                   20


     D pro_rate        pr            11  2
     D   fdt                          7  0 Const
     D   tdt                          7  0 Const
     D   stdt                         7  0 Const
     D   endt                         7  0 Const
     D   amount                      11  2 Const

      *** beg add ***** 09/15/14 **********************************
     d GetLgDedInfo    pr
     d  percent                            like(EXdper)
     d  amtPerClm                          like(EXdclm)
     d  amtAgg                             like(EXdagg)
     d  dedType                            like(EXdtyp)
     d GetSmDedInfo    pr
     d  percent                            like(EXdper)
     d  amtPerClm                          like(EXdclm)
     d  amtAgg                             like(EXdagg)
     d  dedType                            like(EXdtyp)
      *** end add ***** 09/15/14 **********************************


      *** beg chg ***** 11/30/12 ************************************
     D*INDX******      C                   CONST(9)                             array index const
     D expASize        C                   CONST(9)                             array index const
     D rtArSize        c                   const(8)
      *** end chg ***** 11/30/12 ************************************


     D Lower           C                   CONST('abcdefghijklmnopqrstuvwxyz')
     D Upper           C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

     D*  Arrays
     D                 DS
     D expMod                  1     15  4 dim(3)
     D  el2exm1                1      5  4
     D  el2exm2                6     10  4
     D  el2exm3               11     15  4
      *** beg chg ***** 11/30/12 ***********************************************
     D***********      DS
     D*effMod****              1     21  0 dim(3)
     D**el2emd1**              1      7  0
     D**el2emd2**              8     14  0
     D**el2emd3**             15     21  0

     D*rate_dt***      s              7  0 dim(3)
     D*othExp****      s             12  2 dim(INDX)                            Exposure-not payroll
     D*lvl*******      s              1    dim(35) CTDATA PerRcd(35)            Reporting levels
     D*splt_dt***      s              7  0 dim(5) ascend
     D*expamnt***      s             11  2 dim(5) inz
     D*pramt*****      s             11  2 dim(5) inz
      ***********
     D***arrays*for spliting employer payrolls by periods
     D*pefdt*****      s              7  0 dim(5) inz
     D*pexmd*****      s              5  4 dim(5) inz
     D*ratedt****      s              7  0 dim(5) inz
      ******BEG*CHG ******* 03/20/02 *********************************************
     D*manrt******     s              5  2 dim(5) inz
     D*manrt*****      s              7  2 dim(5) inz
      ******END*CHG ******* 03/20/02 *********************************************
     D*splitp****      s              1  0 dim(5) inz
     D*rttier****      s              2  0 dim(5) inz
     D*expact****      s              2  0 dim(5) inz
     D*expos#****      s              2  0 dim(5) inz

     d                 ds
     d effMod                  1     21  0 dim(3) ascend  inz
     d  EL2emd1                1      7  0
     d  EL2emd2                8     14  0
     d  EL2emd3               15     21  0
      *** beg add ***** 01/07/13 ***********************************************
     d effModWrk               1     21  0 dim(3) ascend  inz
     d  effMod1                1      7  0
     d  effMod2                8     14  0
     d  effMod3               15     21  0
      *** end add ***** 01/07/13 ***********************************************

     D rate_dt         s              7  0 dim(rtArSize) inz
     D othExp          s             12  2 dim(ExpASize)                        Exposure-not payroll
     D lvl             s              1    dim(35) CTDATA PerRcd(35)            Reporting levels
     D splt_dt         s              7  0 dim(rtArSize) inz ascend
     D expamnt         s             11  2 dim(rtArSize) inz
      *** beg chg ****** 10/05/12 **********************************
     D*pramt*****      s             11  2 dim(rtArSize) inz
     D pramt           s             12  2 dim(rtArSize) inz
      *** end chg ****** 10/05/12 **********************************
     D pefdt           s              7  0 dim(rtArSize) inz
     D pexmd           s              5  4 dim(rtArSize) inz
     D ratedt          s              7  0 dim(rtArSize) inz
     D**** Beg Del **** 06/17/24 **************************************
     D***manrt           s              7  2 dim(rtArSize) inz
     D**** End Del **** 06/17/24 **************************************
     D splitp          s              1  0 dim(rtArSize) inz
     D rttier          s              2  0 dim(rtArSize) inz
     D expact          s              2  0 dim(rtArSize) inz
     D expos#          s              2  0 dim(rtArSize) inz
     D qqrdat          ds
     D  rDatNum                       7  0 dim(8)
     D qqpdat          ds
     D  pDatNum                       7  0 dim(8)
      *** end chg ***** 11/30/12 *************************************************


     D*  Date/time data structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D*  Date field definitions
     D fromDt          s               d   datfmt(*iso)
     D toDate          s               d   datfmt(*iso)
     D rptDate         s               d   datfmt(*iso)
     D Anniv_dt        s               d   datfmt(*iso)
     D Pol_eff         s               d   datfmt(*iso)
     D date1           s               d   datfmt(*iso)
     D date2           s               d   datfmt(*iso)
     D usaDate         s               d   datfmt(*usa)


     D*  key fields
     D kkclas          s                   like(epclas)
      ***** BEG ADD ***** 02/06/01 *********************************************************
     D kkext           s                   like(pxext)
      ***** END ADD ***** 02/06/01 *********************************************************
     D kkco#           s                   like(fnco#)
     D kkdcde          s                   like(eldcde)
     D kkdcef          s                   like(eldcef)
     D kkdiv           s                   like(eldiv)
     D kkefdt          s                   like(prefdt)
     D kkemp#          s                   like(elemp#)
     D kkfnd           s                   like(elfnd)
     D kkfyr           s                   like(elfyr)
     D kkkey           s                   like(stkey)
     D kkmccase        s                   like(mccase)
     D kktbid          s                   like(gttbid)
     D kkpfdt          s                   like(elpfdt)
     D kkrtcd          s                   like(rsrtcd)
     D kkspec          s                   like(epspec)
     D kkst            s                   like(rsjrst)
     D kkstcd          s              2  0

     D  isInterSt      s              1
     D  asgnRisk       s              1
     D  isRetroRat     s              1
     D  isMCO          s              1
     D  threeYr        s              1
     D  isMultiSt      s              1
     D  estExp         s              1
     D  canxMid        s              1
     D*
     D*  Deductible type data structures
     D Ded_type        DS
     D  Ded_ty12               1      2
     D  Ded_ty34               3      4
     D
     D*
     D*  Deductible reserve codes
     D                 DS
     D dedRsvCod               1     20
     D  dddc01                 1      2
     D  dddc02                 3      4
     D  dddc03                 5      6
     D  dddc04                 7      8
     D  dddc05                 9     10
     D  dddc06                11     12
     D  dddc07                13     14
     D  dddc08                15     16
     D  dddc09                17     18
     D  dddc10                19     20

      *** beg add ***** 09/15/14 *******************
     D*  Large deductible reserve codes
     D                 DS
     D LgDedRsvCds             1     20
     D  LDr101                 1      2
     D  LDr102                 3      4
     D  LDr103                 5      6
     D  LDr104                 7      8
     D  LDr105                 9     10
     D  LDr106                11     12
     D  LDr107                13     14
     D  LDr108                15     16
     D  LDr109                17     18
     D  LDr110                19     20
      *** end add ***** 09/15/14 *******************

     D*  Loss condition codes (from generic table file)
     D                 DS
     D gtcf01                  1     10
     D  act                    1      2
     D  typ_los                3      4
     D
     D*       calc_prem       s             11  2                                      calc premium
     D* prem_ttl        s             11  2                                      prem total - emplyr
     D* mod_prem        s             11  2                                      modified prem - emp
     D****  nDays           s              3  0                                      nmbr of days
     D* xtra_prem       s              9  0                                      excess minimum prem
     D* in_adjust       s             11  2                                      IN flat rt adjstmnt
     D*   ind_amt         s              9  0                                      hold variable

     D xxctystzip      s             36                                         city,state,zip
     D adtPyrl         s              1                                         audited payroll
     D juris_St        s                   like(RSJRST)                         jurisdiction state
     D stcode          s              2  0
     D beg_dt          s              7  0
     D end_dt          s              7  0
     D idx             s              3  0
     D expa            s                   like(epeapr)
     D expo            s              2  0
     D prem            s                   like(epeapr)
     D curLvl          s              2  0                                      current level
     D indemRem        s             10  2                                      indemnity remaining
     D indemPay        s             10  2                                      indemnity pay resrv
     D medicRem        s             10  2                                      medical remng resrv
     D medicPay        s             10  2                                      medical pay resrv
     D allocRem        s             10  2                                      allocated rem resrv
     D allocPay        s             10  2                                      allocated pay resrv
     D sum_arr         s             10  2                                      sum PayRsv array
     D begDt           s                   like(epefdt)
     D endDt           s                   like(epefdt)
     D cxDate          s                   like(emcad1)
     D hold_jrst       s                   like(rsjrst)
     D hold_clm#       s                   like(mcclm#)
     D cat#            s              2  0                                      catastrophe number
     D exp#            s              2  0
     D i               s              2  0                                      counter
     D j               s              1  0                                      index
     D k               s              1  0                                      index
     D m               s              3  0
     D n               s              3  0
     D r               s              3  0
     D s               s              3  0
     D t               s              3  0
      ***** BEG ADD ***** 02/27/04 ************************************
     D x               s              3  0                                      index-Hex Loop
      ***** END ADD ***** 02/27/04 ************************************
     D NJaddr          s             50                                         NewJersey address
     D percap          s              1
     D xxzip           s              9
     D tier            s              2  0
     D diff            s              5  0
     D @@emp#          s              9
     D @@fnd           s              3
     D @@fyr           s              3
      ***** BEG ADD *****  10/09/03  ********************************************
     D @@date          s              8
     D xxdate          s              8  0
      ***** END ADD *****  10/09/03  ********************************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
     D XXEHDT          s              7  0
      ****** END ADD ***** 11/15/04 ************************************
      *
     D***** BEG ADD *****  03/05/02  ********************************************
     D #wrka5          s              5
     D***** END ADD *****  03/05/02  ********************************************
     D***** BEG ADD *****  03/10/04  ********************************************
     D @@styr          s              3  0
     D***** END ADD *****  03/10/04  ********************************************


     DPSDS           ESDS                  Extname(SZQ1P)

    C*
      ***** BEG ADD *****  10/09/03  ********************************************
     C     *entry        plist
     C                   parm                    @@date                         mmddccyy
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@styr
     C***** END ADD ******* 03/10/04 **********************************
     C                   move      @@date        XXDATE
      ***** END ADD *****  10/09/03  ********************************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
     C                   eval      xxehdt =  dateTo7(cvtToDate(xxdate))
      ****** END ADD ***** 11/15/04 ************************************
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Check if WA726R exists in environment
     C                   MOVEL(P)  'WA726R'      XXPGM
     C                   MOVEL     '*PGM'        XXTYP             7
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM            10
     C                   PARM                    XXTYP
     C                   PARM                    XXOLDF            1
     C* If XXOLDF = '1' then use old process
     C* If XXOLDF = '0' then use new API WA726R
     C*** END   ADD ****** 01/04/11 ************************************
     C***** begin add ******* 11/25/10 **********************************
     C                   movel     @@styr        @@yrs             3
     C***** end   add ******* 11/25/10 **********************************
      *
     C*  Key lists
     C
     C*   employer detail
     C     elky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
     C     elky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C*
     C     elky3         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkfyr

     C*   employer master
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv

     C*   group auxiliary file
     C     f2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd

     C*   rate set code master file
     C     rsky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd

     C*   state code table file
     C     stky          klist
     C                   kfld                    kkkey

     C*   employer history file
     C     ehky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C**** Begin Add ****  07/22/05  **************************************
     C*
     C*   employer carrier file
     C     rpky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     C*
     C                   call      'RTVCLTID'
     C                   parm                    cltid             3
     C**** End Add ******  07/22/05  **************************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * Key for check record in future
     C     ehky2         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
      ****** END ADD ***** 11/15/04 ************************************

     C*   table file
     C     tbky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey

     C*   deductible discount code file
     C     ddky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkdcde
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*************      KFLD                    kkdcef
     C                   KFLD                    XXDDT             7 0
     C*** END CHG *****  04/10/06  ************************************
      ***** BEG ADD ***** 06/13/02 ********************************************************

     C*   deductible discount code file -
     C     ddky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkdcde
      ***** END ADD ***** 06/13/02 ********************************************************

     C*   employer payroll file
     C     epky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C*
     C     epky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkclas
     C                   kfld                    kkspec
     C*
     C     epky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#

     C*   payroll master file
     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd
     C                   kfld                    kkclas
     C                   kfld                    kkefdt
     C*
     C     prky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd
     C                   kfld                    kkclas
      ***** BEG ADD ***** 02/06/01 *********************************************************

     C     pxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkclas
     C                   kfld                    kkext
     C                   kfld                    kkst
     C*** BEGIN ADD ***  06/01/06  ************************************
     C                   KFLD                    kkefdt

     c     pxky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkclas
     C                   kfld                    kkext
     C                   kfld                    kkst
     C*** END ADD *****  06/01/06  ************************************

      ***** END ADD ***** 02/06/01 *********************************************************

     C*   statistical rate reporting file
     C     rrky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkefdt

     C*   worker's comp limits table file
     C     lmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkkey
     C                   kfld                    kkpfdt
     C*
     C     lmky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkkey

     C*   claim master file
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv

     C*   multiple claim file
     C     mcky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkmccase

     C*   generic table id file
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey

     C*   occupation code table file
     C     ocky          klist
     C                   kfld                    kkco#

      ***** BEG ADD ***** 08/11/04 *********************************************

     C*   Excluded Employer file
     C     XPKY          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
      ***** END ADD ***** 08/11/04 *********************************************

     C                   exsr      srdriver

     C*====================================================================
     C*  mainloop - process all policies that fall within the date range
     C*             for the current level
     C*====================================================================
     C     mainloop      begsr
     C
     C*  Process all groups except test groups
     C                   read      pfn01                                  70
     C                   dow       *in70 = *off
     C                   if         fntest <> 'Y'
      **** begin add **** 10/04/01 *************************************************************
     C                             and fncicf = 'Y'
      **** end add ****** 10/04/01 *************************************************************
     C
     C                   eval       kkco# = fnco#
     C                   eval       kkfnd = fnfnd
     C
     C*  Process all policies in group
     C     elky          chain     pel01                              60
     C                   dow       *in60 = *off

      *** beg add ***** 11/30/12 *************************************
     c                   if        ELPTDT < ELPFDT
     C     elky          reade     pel01                                  60
     c                   iter
     c                   endif
      *** end add ***** 11/30/12 *************************************

     C                   eval      kkemp# = elemp#
     C                   eval      kkdiv = eldiv

      ***** BEG ADD ***** 08/11/04 *********************************************
      * Check for Excluded Employer, if so bypass output & read next record
     C     xpky          chain     pxp01                              65
     c     *in65         cabeq     *off          NEXT
      ***** END ADD ***** 08/11/04 *********************************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * If policy from date is past the run date, check for a record in
      * WDEHL, if no record exists, read another record.
     C                   If        elpfdt > xxehdt
     C     ehky2         chain     peh01                              61
      ***** BEG CHG ***** 08/12/05 ********************************************
     C**** *in61 ******  cabeq     *on           NEXT
     C                   IF        *IN61 AND cltid <> '093'
     C                   goto      NEXT
     C                   EndIf
      ***** END ADD ***** 08/12/05 ********************************************
     C                   EndIF                                                  End elpfdt > xxehdt
      ****** END ADD ***** 11/15/04 ************************************

     C***** begin add ******* 11/25/10 **********************************
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Process using API only if exists in environment. Else assume old processing
     C                   if        xxoldf = '0'
     C*** END   ADD ****** 01/04/11 ************************************
     C*
     C* Retrieve start policy year based on extarct start date.
     C*
     C                   movel     *zeros        @@styr
     C                   movel     *blanks       xxstyr
     C                   movel     elco#         x1co#             3
     C                   movel     elfnd         x1fnd             3
     C*
     C                   call      'WA726R'
     C                   parm                    x1co#
     C                   parm                    x1fnd
     C                   parm                    @@yrs
     C                   parm                    xxstyr            3
     C                   parm                    @@rtcd            1
     C*
     C     @@rtcd        ifeq      'Y'
     C*
     C                   movel     xxstyr        @@styr            3 0
     C                   else
     C*
     C                   movel     000           @@styr
     C                   endif
     C*
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C                   endif                                                 XXOLDF='0'
     C*** END   ADD ****** 01/04/11 ************************************
     C***** end   add ******* 11/25/10 **********************************
     C*
     C*    Only gather info for the state reporting div
     C     emky          chain     pem01                              61
     C                   if        *in61 = *off
     C                             and emstrp = 'Y'
     C**** begin add **** 03/10/04 *************************************************************
     C                             and elfyr >= @@styr
     C**** end add ****** 03/10/04 *************************************************************
      **** begin add **** 07/25/01 *************************************************************
     C                   clear                   pex01
      **** end   add **** 07/25/01 *************************************************************

     C*      -policy number
     C**** Begin Add ****  07/22/05  **************************************
     C                   eval              exponr = *blank
     C                   if        cltid = '135'
     C     RPKY          CHAIN     PRP01                              63
     C                   if        *in63 = *off and
     C                             rpcap# <> *blank
     C                   movel(P)  rpcap#        exponr
     C                   ENDIF                                                 e 62 OFF...&
     C                   ENDIF                                                 e CLTID=135
     C                   if        exponr = *blank
     C**** End Add ******  07/22/05  **************************************
     C                   move      elfnd         @@fnd
     C                   move      elemp#        @@emp#
     C                   move      elfyr         @@fyr
     C                   eval      exponr = @@fnd + @@emp# + @@fyr
     C**** Begin Add ****  07/22/05  **************************************
     C                   ENDIF                                                 e 62 OFF...&
     C**** End Add ******  07/22/05  **************************************
     C
     C*    Cycle through all divs
     C                   eval       kkfyr = elfyr
     C     elky2         chain     pel02                              62
     C                   dow       *in62 = *off
     C                   eval       kkdiv = el2div
     C
     C*      -exposure state
     C                   eval      exjrst = ' '
     C                   move      el2rtcd       kkrtcd
     C     rsky          chain     prs01                              60
     C  N60              eval       exjrst = rsjrst
     C                   eval       juris_st = exjrst
     C*
     C*
     C*      -multi state policy (y/n)
     C                   eval       isMultiSt = 'N'
     C                   eval       hold_jrst = juris_st
     C                   eval       adtPyrl = *on
     C     elky3         chain     pel03                              60

     C                   dow       *in60 = *off
     C                   move      el3rtcd       kkrtcd
     C     rsky          chain     prs01                              60
     C                   if        (*in60=*off)  AND  (rsjrst <> hold_jrst)
     C                   eval       isMultiSt = 'Y'
     C                   endif
     C*
     C                   if        el3faf <> 'A'
     C                   eval       adtPyrl = *off
     C                   endif
     C*
     C     elky3         reade     pel03                                  60
     C                   enddo                                                  *in60=*off

     C*      -interstate rated (y/n)
     C                   eval       isInterSt = 'N'
     C                   do        3             i
     C                   if        ExpMod(i) <> 0 and ExpMod(i) <> 1
     C                   eval       isInterSt = 'Y'
     C                   endif
     C                   enddo
     C
     C*      -cancelled mid term (y/n)
     C*          Cancelled policy
     C                   eval      cxDate = GetCxDate(elco#:elfnd:elemp#:eldiv)
     C                   if        cxDate = 0
     C                   eval       canxMid = 'N'
     C                   else
     C                   if        cxDate < elptdt
     C                   eval       canxMid = 'Y'
     C                   else
     C     ehky          setgt     peh01
     C     ehky          readpe    peh01                                  60
     C                   if        *in60=*off
     C                             and ehptdt > cxDate
     C                   eval       canxMid = 'Y'
     C                   else
     C                   eval       canxMid = 'N'
     C                   endif                                                  *in60=*off
     C                   endif                                                  cxDate<elptdt
     C                   endif                                                  cxDate=0

     C*      -retrospective rated (y/n)
      **** begin chg **** 07/25/01 *************************************************************
     C****************** movel(p)  elbscd        kkkey
     C                   movel(p)  el2bscd       kkkey
      **** end   chg **** 07/25/01 *************************************************************
     C     tbky          chain     pbs01                              60
     C                   if        bslpgm = 'P' or bslpgm = 'I'
     C                   eval       isRetroRat = 'Y'
     C                   else
     C                   eval       isRetroRat = 'N'
     C                   endif

     C*      -managed care organization (y/n)
      **** begin chg **** 07/25/01 *************************************************************
     C****************** if        elmccd = '00'
     C                   if        el2mccd = '00'
      **** end   chg **** 07/25/01 *************************************************************
     C                   eval       isMCO = 'N'
     C                   else
     C                   eval       isMCO = 'Y'
     C                   endif

     C*      -three year fixed (y/n)
     C                   eval      date1 = cvtToDate(el2pfdt)
     C                   eval      date2 = cvtToDate(el2ptdt)
     C     date2         subdur    date1         diff:*years
     C                   eval      threeYr = 'N'
     C                   if        diff = 3
     C                   eval      threeYr = 'Y'
     C                   endif
     C
     C*      -assigned risk (y/n)
     C                   eval      asgnRisk = 'N'                               default
     C*
     C*      -risk  id#
     C                   movel     emrsk#        exrid#

     C*      -policy type id code
     C                   eval      exptid = '010101'                              default
      *** beg chg ***** 09/15/14 ***********************************************
     C*******-deductible type code
      *****begin*chg **** 07/25/01 *************************************************************
     C****************** if        eldcde <> *blank
     C****************** move      juris_St      kkst
     C****************** move      eldcde        kkdcde
     C****************** z-add     eldcef        kkdcef
     C***********        if        el2dcde <> *blank
     C***********        move      juris_St      kkst
     C***********        move      el2dcde       kkdcde
     C***********        z-add     el2dcef       kkdcef
      *****end***chg **** 07/25/01 *************************************************************
     C****BEGIN*CHG ***  04/10/06  ************************************
     C****BEG*CHG *** 02/07/11 ****************************************
     C***************    MOVE      EL2DEF        Q1DATE
     C***********        MOVE      EL2DEF        Q1DT7
     C****END*CHG *** 02/07/11 ****************************************
     C***********        MOVE      ELCO#         Q1CO#
     C***********        MOVE      ELFND         Q1FND
     C***********        MOVE      ELFYR         Q1FYR
     C***********        MOVE      ELEMP#        Q1EMP#
     C***********        MOVE      ELDIV         Q1DIV
     C***********
     C***********        CALL      'SREFFD'
     C***********        PARM                    Q1CO#             3
     C***********        PARM                    Q1FND             3
     C***********        PARM                    Q1FYR             3
     C***********        PARM                    Q1EMP#            9
     C***********        PARM                    Q1DIV             5
     C****BEG*CHG *** 02/07/11 ****************************************
     C***************    PARM                    Q1DATE            7
     C***********        PARM                    Q1DT7             7
     C****END*CHG *** 02/07/11 ****************************************
     C***********
     C****BEG*CHG *** 02/07/11 ****************************************
     C***************    MOVE      Q1DATE        XXDDT
     C***********        MOVE      Q1DT7         XXDDT
     C****END*CHG *** 02/07/11 ****************************************
     C****END*CHG *****  04/10/06  ************************************
     C***********
     C***********        clear                   pdd01
     C*****ddky**        setgt     pdd01
      ******BEG*ADD ***** 06/13/02 ********************************************************
     C***********        readp     pdd01                                  60
     C**n60ddky2*        readpe    pdd01                                  60
      ******end*ADD ***** 06/13/02 ********************************************************
     C***********        if         *in60=*off
     C****BEG*ADD *****  10/11/07  ************************************
     c***********                  and (ddstat = 'A')
     C****END*ADD *****  10/11/07  ************************************
     C***********
     C***********        eval       ded_ty12 = getDedTyp(fnco# : dedRsvCod)
     C***********        eval       ded_ty34 = ddplan
     C***********        movel     ded_type      exdtyp
     C***********
     C*******-deductible amount per claim
     C***********        eval      exdclm = ddampo
     C*******-deductible amount aggregate
     C***********        eval      exdagg = ddamag
     C***********
      ******BEG*ADD ***** 06/13/02 ********************************************************
     C***********        ELSE
      **If*don't*have a deductible discount code, blank out Deductible fields from prior record
     C***********        z-add     *zeros        exdclm                         Deduct per claim
     C***********        z-add     *zeros        exdagg                         Deduct per Aggregate
     C***********        move      *blanks       exdtyp                         Deductible Type Code
      ******END*ADD ***** 06/13/02 ********************************************************
     C***********
     C***********        endif                                                  *in60=*off
     C***********
      ******BEG*ADD ***** 06/13/02 ********************************************************
     C***********        ELSE
      **If*don't*have a deductible code, blank out Deductible fields from prior record
     C***********        z-add     *zeros        exdclm                         Deduct per claim
     C***********        z-add     *zeros        exdagg                         Deduct per Aggregate
     C***********        move      *blanks       exdtyp                         Deductible Type Code
      ******END*ADD ***** 06/13/02 ********************************************************
     C***********
     C***********        endif                                                  ELDCDE<>*blank
     C***********
     C*******-deductible percent
      ******beg*del ***** 08/03/01 *********************************************************
      ****************                                                      ****************
     C****************   eval      exdper = 0                               ****************
     C****************   if        ded_ty34 = '04'                          ****************
     C****************             or ded_ty34 = '05'                       ****************
     C****************             or ded_ty34 = '06'                       ****************
     C****************             or ded_ty34 = '07'                       ****************
     C****************             or ded_ty34 = '08'                       ****************
     C****************             or ded_ty34 = '11'                       ****************
      *****begin*chg***** 07/25/01 *************************************************************
     C****************** eval       exdper = elddct                         ****************
     C****************   eval       exdper = el2ddct                        ****************
      *****end***chg***** 07/25/01 *************************************************************
     C****************   endif                                              ****************
      ******end*del ***** 08/03/01 *********************************************************
      ******beg*add ***** 08/03/01 *********************************************************
     C***********        z-add     el2ddct       exdper                     ****************
      ******end*add ***** 08/03/01 *********************************************************

      /free
       //*** get deductible info from small or large deductible as applicable
       Clear EXdper;
       Clear EXdclm;
       Clear EXdagg;
       Clear EXdtyp;
       Select;
       When BSlpgm = 'L';
          GetLgDedInfo(EXdper:EXdclm:EXdagg:EXdtyp);
       Other;
          GetSmDedInfo(EXdper:EXdclm:EXdagg:EXdtyp);
       Endsl;
      /end-free
      *** end chg ***** 09/15/14 ***********************************************

     C*      -division number
      **** begin chg **** 07/25/01 *************************************************************
     C****************** eval      exdiv# = eldiv
     C                   eval      exdiv# = el2div
      **** end   chg **** 07/25/01 *************************************************************
     C*      -division name
     C***  Begin Add  ***  08/12/09  **************************************
     C                   clear                   EMNAME
     C                   clear                   EMNAM2
     C***  End   Add  ***  08/12/09  **************************************
      **** beg chg ***** 11/30/12 *************************************
     C*****emky***       chain     pem01                              71
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     c************       if        NOT *in71
     c************       movea     *blanks       hx
     c************       movea     emname        hx
     c************       exsr      srhex
     c************       movea     hx            emname
     C*****Begin*Add  ***  08/12/09  **************************************
     c************       movea     *blanks       hx
     c************       movea     EMNAM2        hx
     c************       exsr      srhex
     c************       movea     hx            EMNAM2
     C*****End***Add  ***  08/12/09  **************************************
     c************       endif
      ***** END ADD ***** 02/27/04 ************************************
     C***  Begin Chg  ***  08/12/09  **************************************
     C**N71*****         eval       exdivn = emname
     c**********         eval       EXDIVN = EMNAME + EMNAM2

     c     emky          chain     pem01
     c                   eval      EXdivn = EMname + EMnam2
     c                   callp     rmvHexChar(EXdivn)
      **** end chg ***** 11/30/12 *************************************
     C***  End   Chg  ***  08/12/09  **************************************
     C*
     C*      -as of date
      ***** BEG CHG *****  10/09/03  ********************************************
     C**************     eval      exaodt =  alphaUSA(cvtToDate(dsdate))
     C                   eval      exaodt =  alphaUSA(cvtToDate(xxdate))
      ***** END CHG *****  10/09/03  ********************************************
     C*

     C                   eval      exmust = isMultiSt
     C                   eval      exisrt = isInterSt
     C                   eval      excxmt = canxMid
     C                   eval      exretr = isRetroRat
     C                   eval      exmco  = isMCO
     C                   eval      ex3yrf = threeYr
     C                   eval      exarsk = asgnRisk
     C
     C                   if        adtPyrl = *on
     C                   eval       exestx = 'N'
     C                   else
     C                   eval       exestx = 'Y'
     C                   endif
     C
     C                   eval      exebas = *blanks

      /free
       //***** beg add *******12/03/13 ************************
       kkRtCd = EL2rtcd;
       //***** end add *******12/03/13 ************************
      /end-free

     C                   exsr      exposure
     C
      *** beg del ***** 12/20/12 ******************************
     C***********        exsr      othExpArr
     C***********
      ****beg*chg ***** 11/30/12 ******************************
     C***********        do        INDX          i
     C***********        do        expASize      i
      ****end*chg ***** 11/30/12 ******************************
     C***********        if        othExp(i) <> 0
     C***********        exsr      wrtOthExp
     C***********        endif
     C***********        enddo
     C***********        clear                   othexp
      *** end del ***** 12/20/12 ******************************

     C                   eval      kkemp# = el2emp#
     C                   eval      kkfyr = el2fyr
     C                   eval      kkstcd = stcode
     C

     C
     C     elky2         reade     pel02                                  62
     C                   enddo                                                  *in62=*off
     C
     C                   endif                                                  *in61=*off

      ***** BEG ADD ***** 08/11/04 *********************************************
     C     NEXT          tag
      ***** END ADD ***** 08/11/04 *********************************************

     C                   eval      *in91 = *off
     C
     C     elky          reade     pel01                                  60
     C                   enddo                                                  *in60=*off
     C
     C                   endif                                                  fntest<>'Y'
     C                   read      pfn01                                  70
     C                   enddo                                                  *in70=*off
     C
     C                   endsr                                                  mainloop



     C*==========================================================================
     C*  Exposure - Subroutine to get exposure record info.  Multiple exposure
     C*             records can be written for each employer based on class code
     C*             experience mod date, rate date, and manual rate.
     C*==========================================================================
     C     Exposure      begsr
     C*
     C                   exsr      Rate_array
     C                   exsr      splt_array

     C*    Get the aircraft surcharge code
     C     rrky          setgt     prr01
     C                   readp     prr01                                  60
     C                   if        *in60=*on
     C                   eval       rrdccd = 0
     C                   endif

     C*
     C     epky          chain     pep01                              62
     C                   dow       *in62 = *off
     C
     C*     Only process if class code is active and rate does not equal 0
     C                   if        (epstat = 'A')
      *** beg del ***** 10/16/13 ************************************
     C*****************            and eparat <> 0
      *** end del ***** 10/16/13 ************************************
      *** beg add ***** 11/30/12 ************************************
     c                             and EPefdt >= EL2pfdt
     c                             and EPefdt <= EL2ptdt
      *** end add ***** 11/30/12 ************************************

     C*      -exposure amount
     C                   if        adtPyrl = *on
     C                   eval      expa = epaapr
     C                   else
     C                   eval      expa = epeapr
     C                   endif
     C
     C*      -exposure act
     C                   if        prlhcd <> 'Y'
     C                   eval       exact = 02
     C                   else
     C                   eval       exact = 01
     C                   endif
     C
     C*      -per capita & hazard code
     C                   eval      kkclas = epclas
     C                   eval      kkefdt = epefdt
     C                   eval      percap = *off
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  70
     C                   if        *in70 = *off
      ***** beg del ***** 12/11/08 *********************************************************
     C*********          movel     prhzgr        exhaz#
      ***** end del ***** 12/11/08 *********************************************************
     C                   if        prpcf = 'C'
     C                   eval       percap = *on
     C                   endif                                                  prpcf='C'
     C                   endif                                                  *in70=*off

      ***** beg add ***** 10/16/13 *********************************************************
      * Skip payrolls with zero rates unless they are population rated or
      * if there is no payroll table set up for them
     c*** Begin Chg *** 05/01/20 **************************************
     c**********         if        *in70 = *off  and
     c**********                   prpcf <> 'O'  and
     c**********                   EParat = 0    or
     c**********                   *in70 = *on
     c                   if        (*in70 = *off and prpcf <> 'O' and
     c                              EParat = 0 and EPclas <> 12) or
     c                             (*in70 = *on)
     c*** End   Chg *** 05/01/20 **************************************
     c     epky          reade     pep01                                  62
     c                   iter
     c                   endif
      ***** end add ***** 10/16/13 *********************************************************

      *** beg chg ***** 12/20/12 ***********************************************************
     C*******additional load exposure record
     C***********        if        epclas = rrdccd
     C***********        eval      exeamt = expa
      ******beg*chg ****** 03/21/01 ********************************************************
     C***************    move      epclas        exclas
      ******BEG*CHG *****  03/05/02  ********************************************
      **Add*Special Description Code to WHEXP/EXCLAS
     C***********        movel     epclas        exclas
     C***********        movel(P)  epclas        #wrka5
     C*****#wrka5        cat(P)    epspec        exclas
      ******END*CHG *****  03/05/02  ********************************************
      ******end*chg ****** 03/21/01 ********************************************************
     C******Beg*Add ****** 02/20/12 ********************************************************
     C***********
     C***********        movel(P)  epext         exext
     C***********
     C******End*Add ****** 02/20/12 ********************************************************
     C***********        eval      exexmd = expmod(1)
     C***********        eval      exrfdt = alphaUSA(cvtToDate(rate_dt(1)))
     C***********        if        exrfdt = '01/01/0001'
     C***********        eval       exrfdt = alphaUSA(cvtToDate(elpfdt))
     C***********        endif
     C***********
      ******beg*CHG ****** 11/13/01 ********************************************************
      **If*don't*have a Rate Start Date here, then use one from WMEPP
     C***********        if        exrfdt = *BLANKS
     C***********        eval      exrfdt = alphaUSA(cvtToDate(elpfdt))
     C***********        eval      exrfdt = alphaUSA(cvtToDate(epefdt))
     C***********        endif
      ******end*CHF ****** 11/13/01 ********************************************************
     C***********
     C***********        if        elandt <> 0
     C***********        eval       exard = alphaUSA(cvtToDate(elandt))
     C***********        else
     C***********        eval       exard = *blank
     C***********        endif
     C***********
     C***********        eval      exmnrt = eparat
     C***********        eval      expamt = epadld
     C***********        eval      exper = 0
     C***********
     C***********        eval      exown# = epco#
     C***********
      ******BEG*ADD ***** 02/06/01 *********************************************************
      ****beg*chg ***** 11/30/12 ***********************************************************
     C***********        exsr      getCLDS
     c***********        eval      EXclds = getClasDesc
      ****end*chg ***** 11/30/12 ***********************************************************
      ******END*ADD ***** 02/06/01 *********************************************************
     C***********
      ******beg*CHG ****** 11/13/01 ********************************************************
      **If*don't*have a Rate Start Date here, then use one from WMEPP
     C***********        if        exrfdt = *BLANKS
     C***********        eval      exrfdt = alphaUSA(cvtToDate(epefdt))
     C***********        endif
      ******end*CHF ****** 11/13/01 ********************************************************
     C***********
     C***********        write     pex01
     C***********
     C*******regular payroll exposure record
     C***********        else
      *** beg chg ***** 11/30/12 **************************************
     C***********        eval      idx = 1
     C*****epefdt        lookup    splt_dt(idx)                         8080
     C***********
     C***********        eval(h)   prem = (expa * eparat)/100
     C***********        eval      begDt = epefdt
     C***********        eval      endDt = el2ptdt
     C***********        if         epcadt <> 0
     C***********        eval       endDt = epcadt
     C***********        endif
     C***********
     C***********        callp     splitPyrl(epco# : epfnd : epfyr :
     C***********                    epemp# : epdiv : epclas : epspec :
     C***********                    idx : expa : prem : eparat :
     C***********                    epefdt : begDt : endDt : percap)
     C***********        do        5             i
     C****BEG*CHG *** 02/07/11 ****************************************
     C***************    if        expamnt(i) <> 0
     C***********        if        manrt(i) <> 0
     C****END*CHG *** 02/07/11 ****************************************
     C***********
     C***********        eval      exeamt = expamnt(i)
     C******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      epclas        exclas
     C******BEG*CHG *****  03/05/02  ********************************************
     C**Add*Special Description Code to WHEXP/EXCLAS
     C***********        movel     epclas        exclas
     C***********        movel(P)  epclas        #wrka5
     C*****#wrka5        cat(P)    epspec        exclas
     C******END*CHG *****  03/05/02  ********************************************
     C******end*chg ****** 03/21/01 ********************************************************
     C******Beg*Add ****** 02/20/12 ********************************************************
     C***********
     C***********        movel(P)  epext         exext
     C***********
     C******End*Add ****** 02/20/12 ********************************************************
     C***********        eval      exexmd = pexmd(i)
     C***********        eval      exrfdt = alphaUSA(cvtToDate(pefdt(i)))
     C***********        if        exrfdt = '01/01/0001'
     C***********        eval       exrfdt = alphaUSA(cvtToDate(elpfdt))
     C***********        endif
     C***********
     C****BEG*ADD *** 02/07/11 ****************************************
     C***********        if        i <= 4
     C****END*ADD *** 02/07/11 ****************************************
     C***********        if        pefdt(i+1) <> 0
     C***********        eval       exrtdt = alphaUSA(cvtToDate(pefdt(i+1)))
     C***********        else
     C***********        eval       exrtdt = alphaUSA(cvtToDate(elptdt))
     C***********        endif
     C****BEG*ADD *** 02/07/11 ****************************************
     C***********        endif
     C****END*ADD *** 02/07/11 ****************************************
     C***********
     C***********        if        elandt <> 0
     C***********        eval       exard = alphaUSA(cvtToDate(elandt))
     C***********        else
     C***********        eval       exard = *blank
     C***********        endif
     C***********
     C***********        eval      exmnrt = manrt(i)
     C***********        eval      expamt = pramt(i)
     C***********        eval      exper = i-1
     C***********
     C***********        eval      exown# = epco#
     C***********
     C******BEG*ADD ***** 02/06/01 *********************************************************
     C***********        exsr      getCLDS
     C******END*ADD ***** 02/06/01 *********************************************************
     C***********
     C******beg*CHG ****** 11/13/01 ********************************************************
     C**If*don't*have a Rate Start Date here, then use one from WDELP
     C***********        if        exrfdt = *BLANKS
     C***********        eval      exrfdt = alphaUSA(cvtToDate(epefdt))
     C***********        endif
     C******end*CHF ****** 11/13/01 ********************************************************
     C***********
     C***********        write     pex01
     C***********
     C***********
     c***********        endif                                                  expamnt(i)<>0
     C***********        enddo                                                  do 5 i
     c***********        callp     splitClass
      *** end chg ***** 11/30/12 **************************************
     C***********        endif                                                  epclas=rrdccd
     c                   callp     splitClass('P')
      *** end chg ***** 12/20/12 **************************************
     c                   exsr      clrPyrlArr

     C                   endif                                                  epstat='A'
     C
     C     epky          reade     pep01                                  62
     C                   enddo                                                  *in61=*off
     C
      *** beg add ***** 12/20/12 **************************************
     c                   callp     splitClass('I')
      *** end add ***** 12/20/12 **************************************
     C*
     C                   endsr                                                  Exposure



     C*===========================================================================
     C*  Rate_array - Subroutine to load array with rate dates
     C*===========================================================================
     C     Rate_array    begsr
     C*
      *** beg chg ***** 11/30/12 ******************************
      ***********
     C***********        eval       rate_dt(1) = el2pfdt
     C***********        eval       rate_dt(2) = el2ptdt
     C***********        eval       rate_dt(3) = *zero
     C***********        if        el2andt <> 0
     C***********                  and el2andt <> el2pfdt
     C***********        eval      Anniv_dt = cvtToDate(el2andt)
     C***********        eval      Pol_eff = cvtToDate(el2pfdt)
     C***********        Adddur    1:*years      Anniv_dt
     C***********        if        Anniv_dt > Pol_eff
     C***********                  and Anniv_dt < cvtToDate(el2ptdt)
     C******cymd*        move      Anniv_dt      rate_dt(2)
     C***********        eval       rate_dt(3) = el2ptdt
     C***********        endif                                                  Anniv_dt>Pol_eff
     C***********        endif                                                  elandt<>0 ...

     c                   move      EL2co#        q1co#
     c                   move      EL2fnd        q1fnd
     c                   move      EL2fyr        q1fyr
     c                   move      EL2emp#       q1emp#
     c                   move      EL2div        q1div
     c                   move      EL2pfdt       q1dt7
     c                   call      'SREFFD'
     c                   parm                    q1co#             3
     c                   parm                    q1fnd             3
     c                   parm                    q1fyr             3
     c                   parm                    q1emp#            9
     c                   parm                    q1div             5
     c                   parm                    q1dt7             7
     c                   parm      *blank        q1type            1
     c                   parm      *blanks       qqclas            5
     c                   parm      *blanks       qqrdat           56
     c                   parm      *blanks       qqpdat           56
     c                   movea     pDatNum       rate_dt
      *** end chg ***** 11/30/12 ***********************************************

     C                   endsr                                                  Rate_array
     C*===========================================================================
     C*  splt_array - Subroutine to load array with rate dates
     C*===========================================================================
     C     splt_array    begsr
     C*
      *** beg chg ***** 11/30/12 ***********************************************
      ***********
     C***********        z-add     9999999       splt_dt
     C***********        eval      splt_dt(1) = rate_dt(1)
     C***********        eval      splt_dt(2) = rate_dt(2)
     C***********        if         rate_dt(3) <> 0
     C***********        eval        splt_dt(3) = rate_dt(3)
     C***********        endif
     C***********        if        ExpMod(2)<>0
     C***********        eval       splt_dt(4) = EffMod(2)
     C***********        endif
     C***********        if        ExpMod(3)<>0
     C***********        eval       splt_dt(5) = EffMod(3)
     C***********        endif
     C***If*more*than one date in array the same, remove all but one
     C***********        do        5             i
     C***********        if        splt_dt(i)<>9999999
     C***********        eval       j=5-i
     C***********        do        j             k
     C***********        if        splt_dt(i)=splt_dt(i+k)
     C***********        eval       splt_dt(i+k)=9999999
     C***********        endif
     C***********        enddo                                                  do j k
     C***********        endif                                                  splt_dt(i)<>9999999
     C***********        enddo                                                  do 5 i
     C***********        sorta     splt_dt

     c                   eval      splt_dt = rate_dt
     c                   if        ExpMod(2) <> 0 and
     c                             %lookup(effMod(2) : splt_dt) = 0
     c                   eval      idx = %lookup(9999999 : splt_dt)
     c                   eval      splt_dt(idx) = effMod(2)
     c                   endif
     c                   if        ExpMod(3) <> 0 and
     c                             %lookup(effMod(3) : splt_dt) = 0
     c                   eval      idx = %lookup(9999999 : splt_dt)
     c                   eval      splt_dt(idx) = effMod(3)
     c                   endif
     c                   sorta     splt_dt
      *** end chg ***** 11/30/12 ***********************************************
      *** beg add ***** 01/07/13 ***********************************************
     c     1             do        3             idx
     c                   eval      effModWrk(idx) = effMod(idx)
     c                   if        effMod(idx) = 0 and idx <> 1
     c                   eval      effModWrk(idx) = 9999999
     c                   endif
     c                   enddo
      *** end add ***** 01/07/13 ***********************************************

     C                   endsr                                                  splt_array
     C*============================================================================
     C*  othExpArr - Subroutine to assign values to non-payroll exposure array
     C*============================================================================
     C     othExpArr     begsr
     C*
     C*  Assign values based on audit status
     C                   if        adtpyrl = *on
     C
     C*                AUDITED
     C*      Deductible discount exposure amt
     C                   add       el2adct       othExp(1)
     C*      Managed care credit exposure amt
     C                   add       el2adcm       othExp(2)
     C*      Safety credit exposure amt
     C                   add       el2adcs       othExp(3)
     C*      Contractor credit exposure amt
     C                   add       el2adc1       othExp(4)
     C*      Deviation exposure amt
     C                   add       el2adc2       othExp(5)
     C*      2nd Deviation exposure amt
     C                   add       el2adv2       othExp(6)
     C*      Schedule debit/credit exposure amt
     C                   add       el2adc        othExp(7)
     C*      Increased limits premium 1
     C                   add       el2ain1       othExp(8)
     C*      Increased limits premium 2
     C                   add       el2ain2       othExp(9)
     C*
     C                   else
     C*
     C*                NOT AUDITED
     C*      Deductible discount exposure amt
     C                   add       el2edct       othExp(1)
     C*      Managed care credit exposure amt
     C                   add       el2edcm       othExp(2)
     C*      Safety credit exposure amt
     C                   add       el2edcs       othExp(3)
     C*      Contractor credit exposure amt
     C                   add       el2edc1       othExp(4)
     C*      Deviation exposure amt
     C                   add       el2edc2       othExp(5)
     C*      2nd Deviation exposure amt
     C                   add       el2edv2       othExp(6)
     C*      Schedule debit/credit exposure amt
     C                   add       el2edc        othExp(7)
     C*      Increased limits premium 1
     C                   add       el2ein1       othExp(8)
     C*      Increased limits premium 2
     C                   add       el2ein2       othExp(9)
     C
     C                   endif                                                  adtpyrl=*on
     C
     C                   endsr                                                  othExpArr



      *** beg del ***** 12/20/12 ***********************************************
      ***********
     C*===========================================================================
     C***wrtOthExp - Writes an exposure record (other than payroll)
     C*===========================================================================
     C*****wrtOthExp     begsr
     C****Class*code
     C***********        select
     C*******Deductible discount exposure amt
     C***********        when      i = 1
      ******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      ddclas        exclas
     C***********        movel     ddclas        exclas
      ******end*chg ****** 03/21/01 ********************************************************
     C***********
     C*******Managed care credit exposure amt
     C***********        when      i = 2                                        managed care credit
      ******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      rr1acd        exclas
     C***********        movel     rr1acd        exclas
      ******end*chg ****** 03/21/01 ********************************************************
     C***********
     C*******Safety credit exposure amt
     C***********        when      i = 3                                        safety credit
      ******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      rr1bcd        exclas
     C***********        movel     rr1bcd        exclas
      ******end*chg ****** 03/21/01 ********************************************************
     C***********
     C*******Contractor credit exposure amt
     C***********        when      i = 4                                        contractor credit
      ******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      rrcccd        exclas
     C***********        movel     rrcccd        exclas
      ******end*chg ****** 03/21/01 ********************************************************
     C***********
     C*******Deviation exposure amt
     C***********        when      i = 5                                        deviation
      ******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      rrdv1c        exclas
     C***********        movel     rrdv1c        exclas
      ******end*chg ****** 03/21/01 ********************************************************
     C***********        if        othExp(i) < 0
     C***********        eval       othExp(i) = -(othExp(i))
     C***********        endif
     C***********
     C*******2nd*Deviation exposure amt
     C***********        when      i = 6                                        2nd deviation
      ******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      rrdv2c        exclas
     C***********        movel     rrdv2c        exclas
      ******end*chg ****** 03/21/01 ********************************************************
     C***********        if        othExp(i) < 0
     C***********        eval       othExp(i) = -(othExp(i))
     C***********        endif
     C***********
     C*******Schedule debit/credit exposure amt
     C***********        when      i = 7                                        schedule dbt/crdt
     C***********        if        othExp(i) > 0
      ******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      rrdbcd        exclas
     C***********        movel     rrdbcd        exclas
      ******end*chg ****** 03/21/01 ********************************************************
     C***********        else
      ******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      rrcrcd        exclas
     C***********        movel     rrcrcd        exclas
      ******end*chg ****** 03/21/01 ********************************************************
     C***********        eval       othExp(i) = -(othExp(i))
     C***********        endif
     C***********
     C*******Increased limits premium 1 & 2
     C***********        when      i = 8                                        increased limits 1
     C***********                  or i = 9
     C***********        eval       exclas = '0000'
     C***********
     C***********        if         i = 8
     C***********        movel(p)  el2ilcd       kkkey
     C***********        else
     C***********        movel(p)  el2inl2       kkkey
     C***********        endif                                                  i=8
      ******BEG*ADD ****** 06/04/02 ********************************************************
     C***********        eval      kkpfdt = *hival
      ******END*ADD ****** 06/04/02 ********************************************************
     C***********
     C*******Get*record for the appropriate limits code
     C*****lmky**        setgt     plm01
     C*****lmky2*        readpe    plm01                                  60
      ******BEG*CHG ****** 06/04/02 ********************************************************
     C*************      dow       (*in60 = *off) and (lmstat = 'I')
     C***********        dou       (*in60)
     C*****lmky2*        readpe    plm01                                  60
      **If*get*limits record, move class and description to output fields
     C***********        IF        (*in60 = *off) and (lmstat <> 'I')
     C***********        movel(P)  lmclas        exclas                         Limits Class Code
     C***********        movel(P)  lmdesc        exelim                         Limits Description
     C***********        movel(P)  lmdesc        exclds                         Class Description
     C***********        endIF
     C*****lmky2*        readpe    plm01                                  60
      ******END*CHG ****** 06/04/02 ********************************************************
     C***********        enddo
      ******beg*chg ****** 03/21/01 ********************************************************
     C**N60********      move      lmclas        exclas
     C**N60********      move      lmdesc        exelim
      ******BEG*DEL ****** 06/04/02 ********************************************************
     C**N60********      movel     lmclas        exclas
     C**N60********      movel     lmdesc        exelim
      ******END*DEL ****** 06/04/02 ********************************************************
      ******end*chg ****** 03/21/01 ********************************************************
     C***********
     C***********        endsl
      ***********
      ******beg*chg ****** 03/21/01 ********************************************************
     C***********        move      exclas        kkclas
     C***********        movel     exclas        kkclas
      ******end*chg ****** 03/21/01 ********************************************************
     C****Exposure coverage code and exposure act
     C*****prky**        setgt     ppr01
     C*****prky1*        readpe    ppr01                                  60
     C***********        if        prlhcd <> 'Y'
     C***********        eval       exact = 02
     C***********        else
     C***********        eval       exact = 01
     C***********        endif
     C***********
      ***********
     C****Process increased limits separately so they can be split
     C***********        if        i <> 8
     C***********                  and i <> 9
      ***********
     C***********        eval       expamt = othexp(i)
     C***********        eval       exeamt = 0
     C***********        eval       exexmd = expMod(1)
     C***********        eval       exrfdt = alphaUSA(cvtToDate(rate_dt(1)))
     C***********        if        exrfdt = '01/01/0001'
     C***********        eval       exrfdt = alphaUSA(cvtToDate(elpfdt))
     C***********        endif
     C***********
     C***********        eval       exrtdt = alphaUSA(cvtToDate(elptdt))
     C***********
     C***********        if         elandt <> 0
     C***********        eval        exard = alphaUSA(cvtToDate(elandt))
     C***********        else
     C***********        eval        exard = *blank
     C***********        endif
     C***********
     C***********        eval       exmnrt = 0
     C***********        eval       exper = 0
     C***********
     C***********        else
     C***If*processing 'increased limits' then split records like payrolls
     C***********        eval       r = 1
     C*****9999999       lookup    splt_dt(r)                             70
     C***********        if        *in70=*on
     C***********        eval       s = r-2
     C***********        else
     C***********        eval       s = 4
     C***********        endif
     C***********
     C***********        do        s             t
     C***********
     C***********        eval(h)   prem = pro_rate(splt_dt(t) :
     C***********                          splt_dt(t+1) : splt_dt(1) :
      *****BEGIN*CHG **** 10/18/01 *****************************************************
     C***********                           splt_dt(r-1) : othexp(i))
     C***********                           splt_dt(s+1) : othexp(i))
      *****END*CHG ****** 10/18/01 *****************************************************
     C***********
     C***********        eval      n=1
     C***********        do        3             m
     C***********        if        splt_dt(t) >= effMod(m)
     C***********                  and effMod(m) <> 0
     C***********        eval       n = m
     C***********        endif
     C***********        enddo
      ***********
     C***********        eval       expamt = othexp(i)
     C***********        eval       exeamt = 0
     C***********        eval       exexmd = expMod(n)
     C***********        eval       exrfdt = alphaUSA(cvtToDate(splt_dt(t)))
     C***********        if        exrfdt = '01/01/0001'
     C***********        eval       exrfdt = alphaUSA(cvtToDate(elpfdt))
     C***********        endif
     C***********
     C***********        eval       exrtdt = alphaUSA(cvtToDate(elptdt))
     C***********
     C***********        if         elandt <> 0
     C***********        eval        exard = alphaUSA(cvtToDate(elandt))
     C***********        else
     C***********        eval        exard = *blank
     C***********        endif
     C***********
     C***********        eval       exmnrt = 0
     C***********        eval       exper = 0
      ***********
     C***********        enddo                                                  s t
      ******BEG ADD ****** 06/04/02 ********************************************************
     C***********        write     pex01
     C***********        move      *BLANKS       exelim
      ******END ADD ****** 06/04/02 ********************************************************
     c***********        endif
      ***********
     C***********        endsr                                                  wrtOthExp
      *** end del ***** 12/20/12 ***********************************************
     C*==========================================================================
     C*  clrPyrlArr - clears the values in the arrays used to accumulate values
     C*               when splitting the payrolls
     C*==========================================================================
     C     clrPyrlArr    begsr

     C                   clear                   expamnt
     C                   clear                   pramt
     C                   clear                   pefdt
     C**** Beg Del **** 06/17/24 **************************************
     C***                clear                   manrt
     C**** End Del **** 06/17/24 **************************************
     C                   clear                   splitp
     C                   clear                   rttier
     C                   clear                   expact
     C                   clear                   expos#
      **** BEGIN ADD **** 09/18/01 *****************************************************
      * Additional Arrays to be Cleared and Logical Switch set off
     C***  begin del  ***  09/19/03  ***************************************
     C****************** eval       adtPyrl = *off
     C***  end   del  ***  09/19/03  ***************************************
     C                   clear                   rate_dt
     C                   clear                   pexmd
     C                   clear                   ratedt
      **** END ADD ****** 09/18/01 *****************************************************
      **** BEGIN chg **** 11/13/01 *****************************************************
     C**********         clear                   othExp
      **** END chg ****** 11/13/01 *****************************************************
     C                   endsr                                                  clrPyrlArr



     H***** BEG ADD ***** 02/06/01 *********************************************************
      *** beg del ***** 11/30/12 ***********************************************************
     C*==========================================================================
     C***getCLDS*     - Load Class description before writing output WHEXP. ach
     C*==========================================================================
     C*****getCLDS       begsr
     C***********
     C***********        eval      kkclas = epclas
     C***********        eval      kkext  = epext
      ******beg*del ***** 08/03/01 *********************************************************
     C*************      eval      kkst   = juris_st
      ******end*del ***** 08/03/01 *********************************************************
      ******beg*add ***** 08/03/01 *********************************************************
     C***********        eval      kkco#  = epco#
     C***********        eval      kkst   = hold_jrst
     C***********        eval      kkrtcd = el2rtcd
      ***********                                                           ****************
     C***********        movel     *blanks       exclds
      ***********                                                           ****************
      ******end*add ***** 08/03/01 *********************************************************
      ******BEG*ADD ***** 08/24/01 *********************************************************
      **Chain*first with Blank State Jurisdiction Code, if not found
      **Chain*again with Held Code, if not found again, use regular desc
      **from*Payroll class file
     C***********        movel     *blanks       kkst
     C****BEGIN*CHG ***  06/01/06  ************************************
     C*****pxky****      chain     ppx01                              60
     C***********        eval      kkefdt = epefdt
     C*****PXKY**        setgt     PPX01
     C*****PXKY1*        readpe    PPX01                                  60
     C****END*CHG *****  06/01/06  ************************************
     C***********        if        (*in60 = *off)
      ***********
      ******BEG*ADD ***** 02/27/04 ************************************
      **Clear*array then Check for Invalid Hex characters
     c***********        movea     *blanks       hx
     c***********        movea     pxdesc        hx
     c***********        exsr      srhex
     c***********        movea     hx            pxdesc
      ******END*ADD ***** 02/27/04 ************************************
     C***********        movel     pxdesc        exclds
     C***********        goto      endCLDS
     C***********        else
     C***********        move      *off          *in60
     C***********        eval      kkst   = hold_jrst
     C***********        endif
      ***********                                                           ****************
      ******END*ADD ***** 08/24/01 *********************************************************
     C****BEGIN*CHG ***  06/01/06  ************************************
     C*****pxky****      chain     ppx01                              60
     C*****PXKY**        setgt     PPX01
     C*****PXKY1*        readpe    PPX01                                  60
     C****END*CHG *****  06/01/06  ************************************
     C***********
     C***********        if        *in60 = *off
      ***********
      ******BEG*ADD ***** 02/27/04 ************************************
      **Clear*array then Check for Invalid Hex characters
     c***********        movea     *blanks       hx
     c***********        movea     pxdesc        hx
     c***********        exsr      srhex
     c***********        movea     hx            pxdesc
      ******END*ADD ***** 02/27/04 ************************************
     C***********        movel     pxdesc        exclds
     C***********
     C***********        else
     C***********
     C***********        eval      kkclas = epclas
     C***********        eval      kkefdt = epefdt
     C*****prky**        setgt     ppr01
     C*****prky1*        readpe    ppr01                                  60
     C***********
     C***********        if        *in60 = *off
      ***********
      ******BEG*ADD ***** 02/27/04 ************************************
      **Clear*array then Check for Invalid Hex characters
     c***********        movea     *blanks       hx
     c***********        movea     prdesc        hx
     c***********        exsr      srhex
     c***********        movea     hx            prdesc
      ******END*ADD ***** 02/27/04 ************************************
     C***********        movel     prdesc        exclds
     C***********        endif
     C***********
     C***********        endif
     C***********
      ******BEG*ADD ***** 08/24/01 *********************************************************
     C*****endCLDS       tag
      ***********
      ******END*ADD ***** 08/24/01 *********************************************************
     C***********        endsr                                                  getCLDS
      ***** END ADD ***** 02/06/01 *********************************************************
      *** end del ***** 11/30/12 ***********************************************************
     C*==========================================================================
     C*  *inzsr - performs all preprocessing necessary to begin extractions
     C*==========================================================================
     C     *inzsr        begsr
     C                   time                    dsdttm
     C                   endsr                                                  *inzsr
     C*==========================================================================
     C*  srdriver - this subroutine drives the process to extract data for each
     C*             reporting level; calls "mainloop" subroutine to get policies
     C*==========================================================================
     C     srdriver      begsr
     C                   exsr      mainloop
     C                   eval      *inlr = *on
     C                   return
     C                   endsr                                                  srdriver
      ***** BEG ADD ***** 02/27/04 ************************************
      *** beg del ***** 11/30/12 ***********************************************************
      ***********
      *================================================================
      **SRHEX*-*Check Alpha input field for Invald Hex characters
      **********The Alpha field is loaded into the arrary w/MOVEA
      **********then Loop thru array & replace w/blank if out of
      **********is not a valid HEX Character. After exiting subr
      **********the array is moved back to the alpha field w/MOVEA
      *================================================================
     c*****srhex*        begsr
     c***********        do        40            x
     c***********        if        (hx(x) < x'40') or
     c***********                  (hx(x) = x'41') or
     c***********                  (hx(x) > x'F9')
     c***********        move      x'40'         hx(x)
     c***********        endif
     c***********        enddo
     c***********        endsr                                                  SROUT
      *================================================================
      ******END*ADD ***** 02/27/04 ************************************
     **Procedures
     P*===========================================================================
     P**splitPyrl - splits payroll records
     P*******Parameters - company#                 3,0
     P***********         fund                     3,0
     P***********         fund year                3,0
     P***********         employer #               9,0
     P***********         division                 5,0
     P***********         payroll class            5,0
     P***********         payroll spec             1
     P***********         index                    3,0
     P***********         exposure amount         11,2
     P***********         premium amount          11,2
     P***********         manual rate              5,2
     P***********         pyrl eff date            7,0
     P***********         beginning date           7,0
     P***********         ending date              7,0
     P***********         per capita flag          1
     P*===========================================================================
     P*splitPyrl*      b                   export
     D*splitPyrl*      pi
     D***##co#***                     3  0 Const
     D***##fnd***                     3  0 Const
     D***##fyr***                     3  0 Const
     D***##emp#**                     9  0 Const
     D***##div***                     5  0 Const
     D***##clas**                     5  0 Const
     D***##spec**                     1    Const
     D***##idx***                     3  0 Const
     D***##eamt**                    11  2
     D***##prem**                    11  2
      ******BEG*CHG ******* 03/20/02 *********************************************
     D***##arat******                 5  2 Const
     D***##arat**                     7  2 Const
      ******END*CHG ******* 03/20/02 *********************************************
     D***##efdt**                     7  0 Const
     D***##beg_dt                     7  0 Const
     D***##end_dt                     7  0 Const
     D***##percap                     1    Const
     D***********
     D**fromdt***      s              7  0
     D**todt*****      s              7  0
      ***********
     C*****epky3*        klist
     C***********        kfld                    ##co#
     C***********        kfld                    ##fnd
     C***********        kfld                    ##fyr
     C***********        kfld                    ##emp#
     C***********        kfld                    ##div
     C***********        kfld                    ##clas
     C***********        kfld                    ##spec
     C***********        kfld                    epext
     C***********        kfld                    ##efdt
     C***********
     C*****epky4*        klist
     C***********        kfld                    ##co#
     C***********        kfld                    ##fnd
     C***********        kfld                    ##fyr
     C***********        kfld                    ##emp#
     C***********        kfld                    ##div
     C***********        kfld                    ##clas
     C***********        kfld                    ##spec
     C***********        kfld                    epext
      ***********
      *****BEGIN*ADD **** 09/18/01 *****************************************************
     C***********        clear                   pep02
      *****END*ADD ****** 09/18/01 *****************************************************
     C***********        eval      fromdt = ##efdt
     C***********        eval      kkefdt = splt_dt(##idx)
     C*****epky3*        setgt     pep02
     C*****epky4*        reade     pep02                                  61
     C***********        if        *in61 = *off
     C***********        eval       todt = ep2efdt
     C***********        else
     C***********        eval       todt = ##end_dt
     C***********        endif
     C***********
     C***********        if        ##idx < 5
     C***********                  and todt > splt_dt(##idx+1)
     C***********        eval(h)    expamnt(##idx) = pro_rate(##beg_dt :
     C***********                                         splt_dt(##idx+1) :
     C***********                                            fromdt : todt :
     C***********                                               ##eamt)
     C***********        eval(h)    ##eamt = ##eamt - expamnt(##idx)
     C***********
     C***********        eval(h)    pramt(##idx) = pro_rate(##beg_dt :
     C***********                                        splt_dt(##idx+1) :
     C***********                                           fromdt : todt :
     C***********                                              ##prem)
     C***********        eval(h)    ##prem = ##prem - pramt(##idx)
     C***********
     C***********        callp     splitPyrl(##co# : ##fnd : ##fyr : ##emp#
     C***********                    : ##div : ##clas : ##spec : ##idx+1 :
     C***********                    ##eamt : ##prem : ##arat : ##efdt :
     C***********                    splt_dt(##idx+1) : ##end_dt : ##percap)
     C***********
     C***********        else
     C***********
     C***********        eval(h)   expamnt(##idx) = expamnt(##idx) + ##eamt
     C***********        eval(h)   pramt(##idx) = pramt(##idx) + ##prem
     C***********        endif
      ***********
     C***********        if        ##percap=*on
     C***********        eval       expamnt(##idx) = 10 * expamnt(##idx)
     C***********        eval       pramt(##idx) = 100 * pramt(##idx)
     C***********        endif
      ***********
      ***********
     C************fill buckets*
     C***********        eval       n = 1
     C***********        do        3             m
     C***********        if        ##beg_dt >= EffMod(m)
     C***********                  and EffMod(m) <> 0
     C***********        eval       n = m
     C***********        endif
     C***********        enddo
     C***********
     C***********        eval      pefdt(##idx) = effMod(n)
     C***********        eval      pexmd(##idx) = expMod(n)
     C***********
     C***********      find rate date based on epefdt
     C***********        eval       n = 1
     C***********        do        3             m
     C***********        if        epefdt >= Rate_dt(m)
     C***********                  and Rate_dt(m) <> 0
     C***********        eval       n = m
     C***********        endif
     C***********        enddo
     C***********
     C***********        eval      ratedt(##idx) = rate_dt(n)
      ***********
     C***********        eval      manrt(##idx) = ##arat
     C***********        eval      splitp(##idx) = ##idx-1
     C***********        move      prtier        rttier(##idx)
     C***********        move      preact        expact(##idx)
     C***********
     C***********        if        prlhcd<>'Y'
     C***********        eval      expos#(##idx) = 11
     C***********        else
     C***********        if        epspec<>'F'
     C***********        eval      expos#(##idx) = 21
     C***********        else
     C***********        eval      expos#(##idx) = 11
     C***********        endif
     C***********        endif
      ***********
     P*splitPyrl*      e
      *** end del ***** 11/30/12 ***********************************************************
     P*===========================================================================
     P* pro_rate - this procedure pro rates the amount passed in using the dates passed
     P*            in to determine the factor:       (tdt - fdt)
     P*                                            ---------------- * amount
     P*                                             (end_dt - stdt)
     P*===========================================================================
     P pro_rate        b                   export
     D pro_rate        pi            11  2
     D   fdt                          7  0 Const
     D   tdt                          7  0 Const
     D   stdt                         7  0 Const
     D   endt                         7  0 Const
     D   amount                      11  2 Const

     D  dtfrom         s               d   datfmt(*usa)
     D  dtto           s               d   datfmt(*usa)
     D  dtstrt         s               d   datfmt(*usa)
     D  dtend          s               d   datfmt(*usa)
     D
     D  xdays          s              5  0
     D  ydays          s              5  0

     C     *cymd         move      fdt           dtfrom
     C     *cymd         move      tdt           dtto
     C     *cymd         move      stdt          dtstrt
     C     *cymd         move      endt          dtend

     C     dtto          subdur    dtfrom        xdays:*days
     C     dtend         subdur    dtstrt        ydays:*days
     C
     C                   return    (xdays/ydays) * amount

     P pro_rate        e



     P*============================================================================
     P*  GetDedTyp - procedure to determine the value for the first two positions
     P*              of Deductible Type in the header record
     P*====================================================================
     P getDedTyp       b                   export
     D getDedTyp       pi             2
     D   company                      3  0
     D   array                       20
     D
     D  dedRsvArr      s              2    dim(10)
     D
     D  ctype          s              3  0 inz(0)
     D  mtype          s              3  0 inz(0)
     D  @@key          s             10
     D  dtype          s              3  0 inz(0)
     D  dedChar        s              2

     C*   table file
     C     tbky2         klist
     C                   kfld                    company
     C                   kfld                    @@key

     C                   eval      dedRsvArr = array

     C                   do        10            i
     C                   if        dedRsvArr(i) <> ' '
     C                   movel(p)  dedRsvArr(i)  @@key
     C*
     C     tbky2         chain     prt01                              60
     C                   if         *in60=*off
     C                   select
     C     rttype        wheneq    'M'
     C                   eval       mtype = 1
     C*
     C     rttype        wheneq    'C'
     C                   eval       ctype = 2
     C*
     C                   endsl
     C                   endif                                                  *in60=*off
     C*
     C                   endif                                                  ded_arr(i)<>' '
     C                   enddo
     C*
     C                   eval       dtype = ctype + mtype
     C                   move(p)   dtype         dedChar
     C
     C                   return     dedChar
     C*
     P getDedTyp       e
     P*** Begin Del *** 05/0/20 ********************************************
     P*=====================================================================
     P* dt6to8 - converts a numeric date in MMYYYY format to MM01YYYY
     P*  Parameter 1 - 6 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     P***dt6to8           B
     D***dt6to8           PI             8  0
     D*** numDate6                       6  0 Const
     D**********
     D*num8*****       s              8  0
     D*alpha6***       s              6
     D*alpha8***       s              8
     C**********
     C**********         move      numDate6      alpha6
     C**********         if        numDate6 > 10000
     C**********         eval      alpha8   = %subst(alpha6:1:2) + '01' +
     C**********                              %subst(alpha6:3:4)
     C**********         else
     C**********         eval      alpha8   = '00' + %subst(alpha6:3:2) +
     C**********                              '01' + %subst(alpha6:5:2)
     C**********         endif
     C**********         move      alpha8        num8
     C*
     C**********         eval      num8 = dtCheck(num8)
     C*
     C**********         return    num8
     C**********
     P***dt6to8           E
     P*** End   Del *** 05/0/20 ********************************************
      *** beg add ***** 11/30/12 ***********************************************
      *** beg chg ***** 12/20/12 ***********************************************
      ***********
      *=========================================================================
      **splitClass - Splits payroll class across exp mod or misc split dates
      *=========================================================================
     p**splitClass*    b
     d*splBegDate      s              7  0
     d*splEndDate      s              7  0
     d*perBegDate      s              7  0
     d*perEndDate      s              7  0
     d*indx******      s              2  0
     d*strIndx***      s              2  0
     d*payRollAmt      s                   like(EPEAPR)
     d*clsPremAmt      s                   like(EPEMPR)
     d*proPayR***      s                   like(EPEAPR)
     d*proPrem***      s                   like(EPEMPR)
     c*****epky**        klist
     c***********        kfld                    EPco#
     c***********        kfld                    EPfnd
     c***********        kfld                    EPfyr
     c***********        kfld                    EPemp#
     c***********        kfld                    EPdiv
     c***********        kfld                    EPclas
     c***********        kfld                    EPspec
     c***********        kfld                    EPext
      ***********
     c*****epky1*        klist
     c***********        kfld                    EPco#
     c***********        kfld                    EPfnd
     c***********        kfld                    EPfyr
     c***********        kfld                    EPemp#
     c***********        kfld                    EPdiv
     c***********        kfld                    EPclas
     c***********        kfld                    EPspec
     c***********        kfld                    EPext
     c***********        kfld                    EPefdt
      **Get*estimated or audited amounts to work with
     c***********        if        adtPyrl = *on
     c***********        eval      payRollAmt = EPaapr
     c***********        eval      clsPremAmt = EPampr
     c***********        else
     c***********        eval      payRollAmt = EPeapr
     c***********        eval      clsPremAmt = EPempr
     c***********        endif
      ***********
      **Establish which payroll period to work with
      ***********
     c***********        eval      perBegDate = EPefdt
     c*****epky1*        setgt     pep02
     c*****epky**        reade     pep02
     c***********        if        not(%eof) and EP2efdt <= EL2ptdt
     c***********        eval      perEndDate = EP2efdt
     c***********        else
     c***********        eval      perEndDate = EL2ptdt
     c***********        endif
      ***********
      **Roll*through all available splits within the period
      **to*prorate the payroll and premium amounts
      ***********
     c***********        eval      strIndx = %lookuple(perBegDate  : splt_dt)
     c*****strIndx       do        rtArSize      indx
     c***********        if        perBegDate = EL2ptdt
     c***********        callp     wrtPayExp(payRollAmt:
     c***********                            clsPremAmt:
     c***********                            perBegDate:
     c***********                            perEndDate:
     c***********                            indx)
     c***********        leave
     c***********        endif
     c***********        if        indx = 8
     c***********        leave
     c***********        endif
     c***********        if        perBegDate <  splt_dt(indx + 1) and
     c***********                  splt_dt(indx + 1) <= EL2ptdt
     c***********        if        perBegDate >= splt_dt(indx)
     c***********        eval      splBegDate  = perBegDate
     c***********        else
     c***********        eval      splBegDate  = splt_dt(indx)
     c***********        endif
     c***********        if        perEndDate <= splt_dt(indx + 1)
     c***********        eval      splEndDate  = perEndDate
     c***********        else
     c***********        eval      splEndDate  = splt_dt(indx + 1)
     c***********        endif
     c***********        if        splBegDate <> splEndDate
     c***********        eval(h)   proPayR  = pro_rate(splBegDate :
     c***********                                      splEndDate :
     c***********                                      perBegDate :
     c***********                                      perEndDate :
     c***********                                      payRollAmt)
     c***********        eval(h)   proPrem  = pro_rate(splBegDate :
     c***********                                      splEndDate :
     c***********                                      perBegDate :
     c***********                                      perEndDate :
     c***********                                      clsPremAmt)
     c***********        callp     wrtPayExp(proPayR :
     c***********                            proPrem :
     c***********                            splBegDate:
     c***********                            splEndDate:
     c***********                            indx)
     c***********        else
     c***********        leave
     c***********        endif
     c***********        endif
     c***********        enddo
     p***splitClass       e
      *=========================================================================
      **wrtPayExp   - Writes payroll class exposure records
      *=========================================================================
     p*wrtPayExp*      b
     d*wrtPayExp*      pi
     d**payRoll**                          like(EPEAPR)
     d**premAmt**                          like(EPEMPR)
     d**spltBegDate                   7  0
     d**spltEndDate                   7  0
     d**index****                     2  0
     d**ix*******      s              2  0
     c***********        if        EParat = 0
     c***********        return
     c***********        endif
     c***********        eval      EXown# = EPco#
     c***********        eval      EXclas = %editc(EPclas : 'X') + EPspec
     c***********        eval      EXext  = EPext
     c***********        eval      EXclds = getClasDesc
     c***********        eval      EXper = index - 1
     C***********        eval      EXmnrt = EParat
     c***********        if        percap=*on
     c***********        eval      EXeamt = 10  * payRoll
     c***********        eval      EXpamt = 100 * premAmt
     c***********        else
     c***********        eval      EXeamt = payRoll
     C***********        eval      EXpamt = premAmt
     c***********        endif
     c***********        eval      EXrfdt =
     c***********                    alphaUSA(cvtToDate(spltBegDate))
     c***********        if        spltEndDate > ELPTDT
     c***********        eval      Exrtdt = alphaUSA(cvtToDate(EL2ptdt))
     c***********        else
     c***********        eval      Exrtdt =
     c***********                    alphaUSA(cvtToDate(spltEndDate))
     c***********        endif
     c***********        if        ELandt <> 0
     c***********        eval      EXard = alphaUSA(cvtToDate(EL2andt))
     c***********        else
     c***********        eval      EXard = *blank
     c***********        endif
     c***********        if        effMod(1) <> 0
     c***********        eval      ix = %lookuple(splt_dt(index) : effMod)
     c***********        if        splt_dt(ix) <> 0
     c***********        eval      EXexmd = expMod(ix)
     c***********        endif
     c***********        endif
     c***********        write     pex01
      ***********
     p*wrtPayExp*      e
      *=========================================================================
      **getClasDesc - Returns class description
      ***********     Chain first with Blank State Jurisdiction Code, if not found
      ***********     Chain again with Held Code, if not found again, use regular desc
      ***********     from Payroll class file
      *=========================================================================
     p*getClasDesc     b
     d*getClasDesc     pi                  like(exCLDS)
     c***********
     c***********        eval      kkclas = epclas
     c***********        eval      kkext  = epext
     c***********        eval      kkco#  = epco#
     c***********        eval      kkst   = hold_jrst
     c***********        eval      kkrtcd = el2rtcd
     c***********        movel     *blanks       exclds
     c***********        movel     *blanks       kkst
     C***********        eval      kkefdt = epefdt
     C*****PXKY**        setgt     PPX01
     C*****PXKY1*        readpe    PPX01                                  60
     C***********        if        (*in60 = *off)
     c***********        callp     rmvHexChar(PXdesc)
     c***********        return    PXdesc
     c***********        else
     c***********        move      *off          *in60
     c***********        eval      kkst   = hold_jrst
     c***********        endif
      ***********
     c*****PXKY**        setgt     PPX01
     c*****PXKY1*        readpe    PPX01                                  60
     c***********        if        *in60 = *off
     c***********        callp     rmvHexChar(PXdesc)
     c***********        return    PXdesc
     c***********        else
     c***********        eval      kkefdt = epefdt
     c*****prky**        setgt     ppr01
     c*****prky1*        readpe    ppr01                                  60
     c***********        if        *in60 = *off
     c***********        callp     rmvHexChar(PRdesc)
     c***********        return    PRdesc
     c***********        endif
      ***********
     c***********        endif
     c***********        return    *blanks
      ***********
     p*getClasDesc     e
      *=========================================================================
      * splitClass - Splits payroll class across exp mod or misc split dates
      *=========================================================================
     psplitClass       b
      *** beg add ***** 12/20/12 ***********************************************
     dsplitClass       pi
     d typeOfClass                    1    const
      *** end add ***** 12/20/12 ***********************************************
     d splBegDate      s              7  0
     d splEndDate      s              7  0
     d perBegDate      s              7  0
     d perEndDate      s              7  0
     d indx            s              2  0
     d strIndx         s              2  0
     d payRollAmt      s                   like(EPEAPR)
     d clsPremAmt      s                   like(EPEMPR)
     d proPayR         s                   like(EPEAPR)
     d proPrem         s                   like(EPEMPR)
     d tableKey        s             10

     c     epky          klist
     c                   kfld                    EPco#
     c                   kfld                    EPfnd
     c                   kfld                    EPfyr
     c                   kfld                    EPemp#
     c                   kfld                    EPdiv
     c                   kfld                    EPclas
     c                   kfld                    EPspec
     c                   kfld                    EPext

     c     epky1         klist
     c                   kfld                    EPco#
     c                   kfld                    EPfnd
     c                   kfld                    EPfyr
     c                   kfld                    EPemp#
     c                   kfld                    EPdiv
     c                   kfld                    EPclas
     c                   kfld                    EPspec
     c                   kfld                    EPext
     c                   kfld                    EPefdt


     c                   select
     c                   when      typeOfClass = 'P'                            payrolls
      * Get estimated or audited amounts to work with
     c                   if        adtPyrl = *on
     c                   eval      payRollAmt = EPaapr
     c                   eval      clsPremAmt = EPampr
     c                   else
     c                   eval      payRollAmt = EPeapr
     c                   eval      clsPremAmt = EPempr
     c                   endif
     c                   if        EPclas = RRdccd
     c                   eval      clsPremAmt = EPadld
     c                   endif
      * Establish which payroll period to work with
     c                   eval      perBegDate = EPefdt
     c     epky1         setgt     pep02
     c     epky          reade     pep02
     c                   dow       not(%eof) and EP2efdt <= EL2ptdt
     c                   eval      strIndx = %lookup(EP2efdt : splt_dt)
     c                   if        strIndx <> 0
     c                   leave
     c                   endif
     c     epky          reade     pep02
     c                   enddo
     c                   if        strIndx <> 0
     c                   eval      perEndDate = EP2efdt
     c                   else
     c                   eval      perEndDate = EL2ptdt
     c                   endif
     c                   callp     procSplit(typeOfClass:
     c                                       tableKey   :
     c                                       payRollAmt :
     c                                       clsPremAmt :
     c                                       perBegDate :
     c                                       perEndDate )
     c                   when      typeOfClass = 'I'                            increased limits
     c                   eval      payRollAmt = 0
     c                   if        adtPyrl = *on
     c                   eval      clsPremAmt = El2ain1
     c                   else
     c                   eval      clsPremAmt = EL2ein1
     c                   endif
     c                   if        clsPremAmt <> 0
     c                   if        EL2ilef >= EL2PFDT
     c                   eval      perBegDate = EL2ilef
     c                   else
     c                   eval      perBegDate = EL2pfdt
     c                   endif
     c                   if        EL2ilex <> 0
     c                   eval      perEndDate = EL2ilex
     c                   else
     c                   eval      perEndDate = EL2ptdt
     c                   endif
     c                   eval      tableKey = EL2ilcd
     c                   callp     procSplit(typeOfClass:
     c                                       tableKey   :
     c                                       payRollAmt :
     c                                       clsPremAmt :
     c                                       perBegDate :
     c                                       perEndDate )
     c                   endif
     c                   if        adtPyrl = *on
     c                   eval      clsPremAmt = El2ain2
     c                   else
     c                   eval      clsPremAmt = EL2ein2
     c                   endif
     c                   if        clsPremAmt <> 0
     c                   if        EL2i2ef >= EL2PFDT
     c                   eval      perBegDate = EL2i2ef
     c                   else
     c                   eval      perBegDate = EL2pfdt
     c                   endif
     c                   if        EL2i2ex <> 0
     c                   eval      perEndDate = EL2i2ex
     c                   else
     c                   eval      perEndDate = EL2ptdt
     c                   endif
     c                   eval      tableKey = EL2inl2
     c                   callp     procSplit(typeOfClass:
     c                                       tableKey   :
     c                                       payRollAmt :
     c                                       clsPremAmt :
     c                                       perBegDate :
     c                                       perEndDate )
     c                   endif
     c                   endsl

     psplitClass       e
      *=========================================================================
      * procSplit - Splits class across date array
      *=========================================================================
     pprocSplit        b
     dprocSplit        pi
     d typeOfClass                    1    const
     d tableKey                      10
     d payRollAmt                          like(EPEAPR)
     d clsPremAmt                          like(EPEMPR)
     d perBegDate                     7  0
     d perEndDate                     7  0
     d splBegDate      s              7  0
     d splEndDate      s              7  0
     d startDate       s              7  0
     d indx            s              2  0
     d strIndx         s              2  0
     d proPayR         s                   like(EPEAPR)
     d proPrem         s                   like(EPEMPR)

      * Roll through all available splits within the period
      * to prorate the payroll and premium amounts

     c                   eval      strIndx = %lookuple(perBegDate  : splt_dt)
     c     strIndx       do        rtArSize      indx
     c                   if        perBegDate = EL2ptdt
     c                   callp     wrtExposure(typeOfClass:
     c                                         tableKey   :
     c                                         payRollAmt :
     c                                         clsPremAmt :
     c                                         perBegDate :
     c                                         perEndDate :
     c                                         indx       )
     c                   leave
     c                   endif
     c                   if        indx = 8
     c                   leave
     c                   endif
     c                   if        perBegDate <  splt_dt(indx + 1) and
     c                             splt_dt(indx + 1) <= EL2ptdt
     c                   if        perBegDate >= splt_dt(indx)
     c                   eval      splBegDate  = perBegDate
     c                   else
     c                   eval      splBegDate  = splt_dt(indx)
     c                   endif
     c                   if        perEndDate <= splt_dt(indx + 1)
     c                   eval      splEndDate  = perEndDate
     c                   else
     c                   eval      splEndDate  = splt_dt(indx + 1)
     c                   endif
     c                   if        splBegDate <> splEndDate
     c                   eval(h)   proPayR  = pro_rate(splBegDate :
     c                                                 splEndDate :
     c                                                 perBegDate :
     c                                                 perEndDate :
     c                                                 payRollAmt)
     c                   eval(h)   proPrem  = pro_rate(splBegDate :
     c                                                 splEndDate :
     c                                                 perBegDate :
     c                                                 perEndDate :
     c                                                 clsPremAmt)
     c                   if        perBegDate > splt_dt(indx)
     c                   eval      startDate = perBegDate
     c                   else
     c                   eval      startDate = splt_dt(indx)
     c                   endif
     c                   callp     wrtExposure(typeOfClass      :
     c                                         tableKey         :
     c                                         proPayR          :
     c                                         proPrem          :
     c                                         startDate        :
     c                                         splt_dt(indx + 1):
     c                                         indx       )
     c                   else
     c                   leave
     c                   endif
     c                   endif
     c                   enddo

     pprocSplit        e
      *=========================================================================
      * wrtExposure - Writes exposure records
      *=========================================================================
     p wrtExposure     b
     d wrtExposure     pi
     d  typeOfClass                   1    const
     d  tableKey                     10
     d  payRoll                            like(EPEAPR)
     d  premAmt                            like(EPEMPR)
     d  spltBegDate                   7  0
     d  spltEndDate                   7  0
     d  index                         2  0
     d  ix             s              2  0
     d  clasCode       s             10
     d  clasDesc       s            100

      /Free
       //  Begin Add  ****** 02/04/14 *********************************
           EXelim = *blanks;
       //  End   Add  ****** 02/04/14 *********************************
      /End-Free
     c                   select

     c                   when      typeOfClass = 'P'
      *** beg del ***** 10/16/13 ******************************
     c***********        if        EParat = 0
     c***********        return
     c***********        endif
      *** end del ***** 10/16/13 ******************************
     c                   eval      EXown# = EPco#
     c                   eval      EXclas = %editc(EPclas : 'X') + EPspec
     c                   eval      EXext  = EPext
     c                   callp     getClasInfo(typeOfClass:
     c                                         tableKey   :
     c                                         clasCode   :
     c                                         clasDesc)
     c                   eval      EXclds = clasDesc
     c                   eval      EXper = index - 1
     c                   eval      EXmnrt = EParat
      *** beg chg ***** 07/02/18 **********************************
     c***                if        percap=*on and EPclas <> RRdccd
     c***                eval      EXeamt = 10  * payRoll
     c***                eval      EXpamt = 100 * premAmt
     c***                else
     c***                eval      EXeamt = payRoll
     C***                eval      EXpamt = premAmt
     c***                endif
     c                   eval      EXeamt = payRoll
     c                   eval      EXpamt = premAmt
      *** end chg ***** 07/02/18 **********************************

     c                   when      typeOfClass = 'I'
     c                   eval      EXown# = EL2co#
     c                   callp     getClasInfo(typeOfClass:
     c                                         tableKey   :
     c                                         clasCode   :
     c                                         clasDesc)
     c                   eval      EXclas = clasCode
     c                   eval      EXclds = clasDesc
     c                   eval      EXelim = clasDesc
     c                   eval      EXext  = *blanks
     c                   eval      EXper = index - 1
     c                   eval      EXmnrt = 0
     c                   eval      EXeamt = 0
     c                   eval      EXpamt = premAmt
     c                   endsl

     c                   eval      EXrfdt =
     c                               alphaUSA(cvtToDate(spltBegDate))
     c*** Begin Chg *** 02/08/22 *******************************************
     c**********         if        spltEndDate > ELPTDT
     c**********         eval      Exrtdt = alphaUSA(cvtToDate(EL2ptdt))
     c**********         else
     c**********         eval      Exrtdt =
     c**********                     alphaUSA(cvtToDate(spltEndDate))
     c**********         endif
     c                   If        EPcaDt <= ELptDt And
     c                             EPcaDt <= spltEndDate And
     c                             EPcaDt <> 0
     c                   eval      Exrtdt = alphaUSA(cvtToDate(EPcaDt))
     c                   Else
     c                   if        spltEndDate <= ELPTDT
     c                   eval      Exrtdt =
     c                               alphaUSA(cvtToDate(spltEndDate))
     c                   else
     c                   eval      Exrtdt = alphaUSA(cvtToDate(EL2ptdt))
     c                   endif
     c                   endif
     c*** End   Chg *** 02/08/22 *******************************************
     c                   if        ELandt <> 0
     c                   eval      EXard = alphaUSA(cvtToDate(EL2andt))
     c                   else
     c                   eval      EXard = *blank
     c                   endif
      *** beg chg ***** 01/07/13 ***********************************************
     c***********        if        effMod(1) <> 0
     c***********        eval      ix = %lookuple(splt_dt(index) : effMod)
     c***********        if        splt_dt(ix) <> 0
     c***********        eval      EXexmd = expMod(ix)
     c***********        endif
     c***********        endif
     c                   eval      ix = %lookuple(splt_dt(index) : effModWrk)
     c                   select
     c                   when      ix = 0
     c                   eval      EXexmd = 0
     c                   other
     c                   eval      EXexmd = expMod(ix)
     c                   endsl
      *** end chg ***** 01/07/13 ***********************************************
     c                   write     pex01

     p wrtExposure     e
      *=========================================================================
      * getClasInfo - Returns class code and class description
      *               Chain first with Blank State Jurisdiction Code, if not found
      *               Chain again with Held Code, if not found again, use regular desc
      *               from Payroll class file
      *=========================================================================
     p getClasInfo     b
     d getClasInfo     pi
     d  typeOfClass                   1    const
     d  tableKey                     10
     d  clasCode                     10
     d  clasDesc                    100

     c                   select
     c                   when      typeOfClass = 'P'
     c                   eval      kkclas = epclas
     c                   eval      kkext  = epext
     c                   eval      kkco#  = epco#
     c                   eval      kkst   = hold_jrst
     c                   eval      kkrtcd = el2rtcd
     c                   movel     *blanks       exclds
     c                   movel     *blanks       kkst
     C                   eval      kkefdt = epefdt
     C     PXKY          setgt     PPX01
     C     PXKY1         readpe    PPX01                                  60
     C                   if        (*in60 = *off)
     c                   callp     rmvHexChar(PXdesc)
     c                   eval      clasDesc = PXdesc
     c                   else
     c                   move      *off          *in60
     c                   eval      kkst   = hold_jrst
     c                   endif

     c     PXKY          setgt     PPX01
     c     PXKY1         readpe    PPX01                                  60
     c                   if        *in60 = *off
     c                   callp     rmvHexChar(PXdesc)
     c                   eval      clasDesc = PXdesc
     c                   else
     c                   eval      kkefdt = epefdt
     c     prky          setgt     ppr01
     c     prky1         readpe    ppr01                                  60
     c                   if        *in60 = *off
     c                   callp     rmvHexChar(PRdesc)
     c                   eval      clasDesc = PRdesc
     c                   endif

     c                   endif

     c                   when      typeOfClass = 'I'
     c                   eval      kkst   = hold_jrst
     c                   movel(p)  tableKey      kkkey
     c                   eval      kkpfdt = EL2pfdt
     c     lmky          setgt     plm01
     c     lmky2         readpe    plm01                                  60
     c                   dow       not(%eof)
     c                   if        lmstat <> 'I'
     c                   movel(p)  lmclas        clasCode                       Limits Class Code
     c                   movel(p)  lmdesc        clasDesc                       Class Description
     c                   leave
     c                   endif
     c     lmky2         readpe    plm01                                  60
     c                   enddo
     c                   endsl

     p getClasInfo     e
      *** end chg ***** 12/20/12 ***********************************************
      *================================================================
      * rmvHexChar -   Check Alpha input field for Invald Hex characters
      *                The Alpha field is loaded into the arrary w/MOVEA
      *                then Loop thru array & replace w/blank if out of
      *                is not a valid HEX Character. After exiting subr
      *                the array is moved back to the alpha field w/MOVEA
      *================================================================
     p rmvHexChar      b
     d rmvHexChar      pi
     d  desc                               like(EXclds) options(*varsize)
     d  hx             s              1    dim(40) inz

     c                   movea     desc          hx
     c                   do        40            x
     c                   if        (hx(x) < x'40') or
     c                             (hx(x) = x'41') or
     c                             (hx(x) > x'F9')
     c                   move      x'40'         hx(x)
     c                   endif
     c                   enddo
     c                   movea     hx            desc

     p rmvHexChar      e
      *================================================================
      *** end add ***** 11/30/12 ***********************************************
      *** beg add ***** 09/15/14 ***********************************************
      *================================================================
      * GetLgDedInfo - Retrieve Large Deductible Information
      *================================================================
     p GetLgDedInfo    b
     d GetLgDedInfo    pi
     d  percent                            like(EXdper)
     d  amtPerClm                          like(EXdclm)
     d  amtAgg                             like(EXdagg)
     d  dedType                            like(EXdtyp)
      /free
       Chain (EL2co#:EL2fnd:EL2emp#:ELdiv:ELfyr) pld01;
       If not(%found);
          Return;
       Endif;
       percent   = LDpct1;
       amtPerClm = LDamp1;
       amtAgg    = LDamg1;
       dedType   = GetDedTyp(FNco#:lgDedRsvCds) + LDpln1;
      /end-free
     p GetLgDedInfo    e
      *================================================================
      * GetSmDedInfo - Retrieve Small Deductible Information
      *================================================================
     p GetSmDedInfo    b
     d GetSmDedInfo    pi
     d  percent                            like(EXdper)
     d  amtPerClm                          like(EXdclm)
     d  amtAgg                             like(EXdagg)
     d  dedType                            like(EXdtyp)
      /free
       If EL2dcde = *blank;
          Return;
       Endif;
       Exsr GetDdkyVals;
       Setgt  ddky  pdd01;
       Readpe ddky2 pdd01;
       If %eof;
          Return;
       Endif;
       percent   = DDpct;
       amtPerClm = DDampo;
       amtAgg    = DDamag;
       dedType   = GetDedTyp(FNco#:dedRsvCod) + DDplan;
      /end-free
      *==================================================
      * GetDdkyVals - Populate key values for chain to WTDDP
      *==================================================
     c     GetDdkyVals   begsr
     c                   move      juris_St      kkst
     c                   move      el2dcde       kkdcde
     c                   move      el2dcef       q1dt7
     c                   move      EL2co#        q1co#
     c                   move      EL2fnd        q1fnd
     c                   move      EL2fyr        q1fyr
     c                   move      EL2emp#       q1emp#
     c                   move      EL2div        q1div
     c                   call      'SREFFD'
     c                   parm                    q1co#             3
     c                   parm                    q1fnd             3
     c                   parm                    q1fyr             3
     c                   parm                    q1emp#            9
     c                   parm                    q1div             5
     c                   parm                    q1dt7             7
     c                   move      q1dt7         xxddt
     c                   endsr
     p GetSmDedInfo    e
      *** end add ***** 09/15/14 ***********************************************


**Ctdata lvl
123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
