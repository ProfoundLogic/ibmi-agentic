     H*  Revisions:                                                   *
     H*                                                               *
     H* 12/15/25 C:65  Support: 120265 (Amwins)                       *
     H*                Load prorated premium discount amount into the *
     H*                state prem record and exposure record.         *
     H*                                                               *
     H* 07/03/25 C:333 Support: Phw#119335   (FCC)                    *
     H*                Subject Premium in Endorsement Calculating     *
     H*                Incorrectly.                                   *
     H*                - Adding tas38 and tms38.                      *
     H*                                                               *
     H* 01/03/25 C:65  Support: PHW#117943 (Amwins)                   *
     H*                Add cancellation date filed to TAST.           *
     H*                                                               *
     H* 07/16/24 C:340 Project: 148WEW (AmWins)                       *
     H*                Large Deductible Form WC990660.                *
     H*                Add Array - amwinsLdForms                      *
     H*                                                               *
     H* 06/20/24 C:65  Support: PHW115225 (SUNZ)                      *
     H*                Add another standard premium field to TAST     *
     H*                data structure to hold prorated standard       *
     H*                for the header records.  Balance to min will   *
     H*                not be included for MA.  Add additonal fields  *
     H*                to hold prorated amounts for each exposure     *
     H*                record to be created.                          *
     H*                                                               *
     H* 06/17/24 C:344 Project: WC2952                                *
     H*                Increase rate field to zoned 8.3.              *
     H*                                                               *
     H* 05/28/24 C:333 Support: PHW115606 (SUNZ)                      *
     H*                WCPOLS - State Specific Policy Page Forms      *
     H*                                                               *
     H* 03/04/24 C:79  Project: WC2951                                *
     H*                Use new Employer Detail Exp Rating field       *
     H*                                                               *
     H* 01/09/24 C:65  Project: Phw#113651 (Amwins)                   *
     H*                Add fields to store 3 installment schedule     *
     H*                form names.                                    *
     H*                                                               *
     H* 12/05/23 C:300 Project: WC2810                                *
     H*                Add TMS27 to report Subject Premium.           *
     H*                Add TASPFTR and TASPDTR to report Prorated     *
     H*                Terrorism Charges                              *
     H*                                                               *
     H* 12/04/23 C:65  Project: WC2810                                *
     H*                TASMIN should be part of TAST data structure.  *
     H*                                                               *
     H* 09/18/23 C:65  Project: 153WAL                                *
     H*                Add definition for safety fee to TAST DS.      *
     H*                                                               *
     H* 08/15/23 C:65  Project: WC2908                                *
     H*                Add definitions for sending escape message.    *
     H*                                                               *
     H* 07/30/23 C:62  Project: WC2918                                *
     H*                Add exp mod 2 and 3 not rated flag logic.      *
     H*                                                               *
     H* 07/07/23 C:300 Project: WC2810                                *
     H*                W7413R program reached 32764 lines and SEU does*
     H*                allow to insert more lines, hence creating this*
     H*                copy book to move all D specs here             *
     H*                                                               *
    H*****************************************************************

     D processMe       s               n   inz(*off)

     D faa             s             40    dim(3)
     D eaa             s             40    dim(3)
    D*
     D pdat            s              7  0 dim(6) ascend
     D rdat            s              7  0 dim(6) ascend
     D mth             s              3  1 dim(12) ctdata perrcd(1) ascend
     D hp              s              4  3 dim(12) alt(mth)
     D con             s             50    dim(80) ctdata perrcd(1)
     D tam             s             11  2 dim(45)
      *** Beg Add *****  07/07/23  *******************************************
     D tam2            s             11  2 dim(45)
     D minBal          s              9  2
     D strpdiv         s              5  0
      *** End Add *****  07/07/23  *******************************************
     D tmp             s              1    dim(50)
     D num             s              1    dim(5)
     D xf              s             18    dim(500)
     D xfSt            s              2    dim(500)
     D xfDesc          s             60    dim(500)
     D xf1             s             35    dim(50)
     D af              s             15    dim(500)
     D dt              s              1    dim(78)
     D cmd             s             80    dim(2) ctdata perrcd(1)
     D qca             s             60    dim(4) ctdata perrcd(1)
     D cmd2            s             80    dim(2) ctdata perrcd(1)
     d flgApprOrExcl   s              1
     d aryLdSt         s              2    dim(50)
     d chrForm8        s              8
     d intBdx          s              3  0
     d intIdx          s              3  0
     d intSdx          s              3  0
     d baseLdForms     s              8    dim(10) ctdata
     d sunzLdForms     s              8    dim(30) ctdata
      *** Beg Add ***** 12/05/22 ********************************
     d vitalLdForms    s              8    dim(10) ctdata
      *** End Add ***** 12/05/22 ********************************
      *** Beg Add ***** 07/15/24 *** 148WEW *********************
     d amwinsLdForms   s              8    dim(30) ctdata
      *** End Add ***** 07/15/24 *** 148WEW *********************
     D saveLocDiv      s                   like(ALDIV)
     D xst             s              2    dim(50) ascend
     D yst             s              2    dim(50) ascend
     D pst             s              2    dim(1000)
     D dv              s              5    dim(500)
     D name            s             56    dim(9)
     D pocSt           s              2    dim(50)
     D ipoc            s              3  0
      /Free
     d decFWCIGATotal  s             11  2
     d conFWCIGACode   c                   9621
      /End-Free
     D xxSeq#          s              5  0
     D addNameFein     s              9  0
     D addNameTpOB     s                   LIKE(ANtpOB) inz
     D sort025         s             11    dim(4)
     D form025         s             10
     D order025        s              1
     d  co#            s                   like(ELCO#)
     d  fnd            s                   like(ELFND)
     d  emp#           s                   like(ELEMP#)
     d  div            s                   like(ELDIV)
     d  xxdiv1         s                   like(eldiv)
     d  xxhdiv         s                   like(eldiv)
     d  x5div          s                   like(eldiv)
     d  xxelid         s                   like(alelid)
     d  xxhlid         s                   like(alelid)
     d  xxhlid1        s             10  0
     d  fyr            s                   like(ELFYR)
     d  svCar#         s                   like(F2CAR#)
     d  agt#           s              5  0
     d  agt_name       s             60
     d  prod           s              5  0
     d  prod_nm        s             60
     d  area_cd        s              3  0
     d  phone#         s              7  0
     d  plan           s              1
     d  asOfDate       s              8  0
     D xxstate         s              2
     D xxRDST          s              2A
     D xxtstc          s              5  0
     D xxtstf          s              5  0
     D xxtfpr          s             11  2
     D xxtapr          s             11  2
     D xxtamt          s             11  2
     D xxtrt           s              5  3
     D xaefdt          s                   like(anefdt)
     D xadldt          s                   like(andldt)
     D xoefdt          s              8  0
     D xodldt          s              8  0
     d cxStatus        s             10
     d xxcx            s              1
     d divCxDate       s              7  0
     d xxdldt          s              7  0
     d xrefdt          s              7  0
     d xrexdt          s              7  0
     D xxcnt           s              3  0 inz(*zeros)
     D xxfst           s               n   inz(*off)
     D monoState       s               n   inz(*off)
     d PremVal        uds                  dtaara(C139WLR)
     d   depprm                1     11
     d   tam10                12     22
     d xxH22           s             22
     d xxH19           s             19
     d xxCtr           s             20
     d xxpremPct       s             12  9 inz(0)
     d xxlossSub       s              2  0 inz(0)
     d xxbasisDed      s              2  0 inz(0)
     d xxdedPct        s              2  0 inz(0)
     d xxdedAmt        s              9  0 inz(0)
     d xxdedAgg        s              9  0 inz(0)
     d xxdedClas       s              5  0 inz(0)
     d xxdedNet        s              1    inz(' ')
     d xxdedVerb       s              1    inz(' ')
     d xxPlan          s              1    inz(' ')
     d xxWrite         s              1    inz(' ')
     d*** Begin Add *** 02/22/22 *******************************************
     d QQname1         s            120
     d QQname2         s            120
     d QQname3         s            120
     d QQname4         s            120
     d QQPEOExt        s             10
     d QQfein          s              9  0
     d*** End   Add *** 02/22/22 *******************************************
     d*** Beg Add ***** 06/02/23 ****************************************
     d strSvP4XML      s                   Like(P4Xml)
     d intLength       s              5  0
     d intCount        s              5  0
     d strForm         s             15a
     d*** End Add ***** 06/02/23 ****************************************
     D                 ds
     D e00403                       150
     D  xxdac                         9  0 overlay(e00403)
     D  xxprp                         5  3 overlay(e00403:10)
     D  xxdtype                       4  0 overlay(e00403:15)
     D  xxlsd                         2  0 overlay(e00403:15)
     D  xxbdc                         2  0 overlay(e00403:17)
     D  xxdpc                         2  0 overlay(e00403:19)
     D  xxdag                         9  0 overlay(e00403:21)
     D  xxalei                        1    overlay(e00403:30)
     D  xxfeci                        1    overlay(e00403:31)
     d                 ds
     d e29601ds                      70
     d  xxbia                        10  0 overlay(e29601ds)
     d  xxbid                        10  0 overlay(e29601ds:11)
     d  xxabi                        10  0 overlay(e29601ds:21)
     d  xxbiap                       10  0 overlay(e29601ds:31)
     d  xxbidp                       10  0 overlay(e29601ds:41)
     d  xxagpo                       10  0 overlay(e29601ds:51)
     d  xxagpp                       10  0 overlay(e29601ds:61)
     d isContri        s              1
     d svState         s                   like(STstate)
     d svStCd          s                   like(STcode)
     d svPrtZrFlg      s                   like(STflag)
     d svStatus        s                   like(STstate)
     d xmlFile         s                   like(PWXML)
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D xxSchName       s            100
     D xxSchFdnm       s            100
     D xxSchAdr1       s                   like(EMMAD1)
     D xxSchAdr2       s                   like(EMMAD2)
     D xxSchCity       s                   like(EMMCTY)
     D xxSchSt         s                   like(EMMST)
     D xxSchZip        s                   like(EMMZIP)
     D xxLossSt        s              2
     D xxLocName       s            120
     D xxLocFdnm       s            120
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('WC')
     d processType     c                   const('Policy Page')
     d svMail          s             60
     d qqMail          s             60
     d saveMail        s             60

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     d order           s              5  0
     d outputFile      s                   like(XPFILE)
     d templateFile    s                   like(XTXMLT)
     d addNameFile     s                   like(XTXMLT)
     d addLocFile      s                   like(XTXMLT)
     d premiumFile     s                   like(XTXMLT)
     d endorseFile     s                   like(XTXMLT)
     d mainSt          s              2
     d sType           s              1
     d xxProcessType   s            100
     d count           s              5  0
     d xxSig           s             60
     d xxSig2          s             60
     d xxFolder        s            250
     d xxFolder2       s            250
     d xxListSchd      s              1
     d declareForm     s             18
     d addNameForm     s             18
     d addLocForm      s             18
     d premiumForm     s             18
     d endorseForm     s             18
     d declareNatFm    s             18
     d addNameNatFm    s             18
     d addLocNatFm     s             18
     d premiumNatFm    s             18
     d endorseNatFm    s             18
     d xxNatFm         s             15
     d declareDesc     s            100
     d addNameDesc     s            100
     d addLocDesc      s            100
     d premiumDesc     s            100
     d endorseDesc     s            100
     d declareDate     s              7  0
     d addNameDate     s              7  0
     d addLocDate      s              7  0
     d premiumDate     s              7  0
     d endorseDate     s              7  0
     d addNameSeq      s              3  0
     d addLocSeq       s              3  0
     d premiumSeq      s              3  0
     d endorseSeq      s              3  0
     d endsForm        s                   like(P4Form)
      *** Beg Add ***** 05/28/24 *****************************
     d declareST       s              2
      *** End Add ***** 05/28/24 *****************************
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const
     d w7413rmpd       pr                  ExtPgm('W7413RMPD')
     d   qqco#_                       3    const
     d   qqFnd_                       3    const
     d   qqEmp#_                      9    const
     d   qqDiv_                       5    const
     d   qqFyr_                       3    const
     d   qqRtcd_                      1
     d* Get the Territorial Differential Factor
     d GetTDF          pr                  extpgm('WA101R')
     d  inCompany#                    3  0
     d  inGroup#                      3  0
     d  inEmployer#                   9  0
     d  inDivision                    5  0
     d  inYear                        3  0
     d  outTDF                        5  4
     d  outTDF         s              5  4
    D*
     D                 ds
     D  grpAdr                 1    700    dim(7)
     D  grpAdr1                1    100
     D  grpAdr2              101    200
     D  grpAdr3              201    300
     D  grpAdr4              301    400
     D  grpAdr5              401    500
     D  grpAdr6              501    600
     D  grpAdr7              601    700
    D*
     D                 ds
     D  emn                    1    450    dim(9)
     D  ##emn1               001    050
     D  ##emn2               051    100
     D  ##emn3               101    150
     D  ##emn4               151    200
     D  ##emn5               201    250
     D  ##emn6               251    300
     D  ##emn7               301    350
     D  ##emn8               351    400
     D  ##emn9               401    450
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
    D*

     D empads          ds
     D  empadr1
     D  empadr2
     D  empadr3
     D  empadr4
     D  empadr5
    D empa                                OVERLAY(empads)
     D                                     LIKE(empadr1) DIM(5)

     D                 ds                  inz
     d  namAddr                1    540    dim(9)
     D  namAdd1                1     60
     D  namAdd2               61    120
     D  namAdd3              121    180
     D  namAdd4              181    240
     D  namAdd5              241    300
     D  namAdd6              301    360
     D  namAdd7              361    420
     D  namAdd8              421    480
     D  namAdd9              481    540
     D                 ds                  inz
     D  adh                    1    144    dim(9)
     D  xxadh1                 1     16
     D  xxadh2                17     32
     D  xxadh3                33     48
     D  xxadh4                49     64
     D  xxadh5                65     80
     D  xxadh6                81     96
     D  xxadh7                97    112
     D  xxadh8               113    128
     D  xxadh9               129    144
    D*
     D xxnam4          ds           360
     D svnam4          ds           360
     D                 DS
     D  XXPCT                  1     12  9
     D  XXP1                   2      3
     D  XXP2                   4     12
    D*
     D emfds           ds
     D emrrn                 397    400b 0
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x@RRN           s              9  0 inz(0)
     d x#Dcid          s             12    inz(' ')
     d x#Fold          s             80    inz(' ')
     d x#Uncn          s            255    inz(' ')
     d x#Key           c                   const('Policy Page')
     D x#Type          c                   const('Policy Set')
     D x#Mode          c                   const('E')
     D isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFileName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d*
     D pinfds        e ds                  extname(szq4p)
     D p1infds       e ds                  extname(szq4p)
     D  p1File       e                     extFld(Q4FILE)
     D  p1Open       e                     extFld(Q4OPEN)
     D  p1Eof        e                     extFld(Q4EOF )
     D  p1StCd       e                     extFld(Q4STCD)
     D  p1OpCd       e                     extFld(Q4OPCD)
     D  p1Rtn        e                     extFld(Q4RTN )
     D  p1SSeq       e                     extFld(Q4SSEQ)
     D  p1RcPr       e                     extFld(Q4RCPR)
     D  p1ExN1       e                     extFld(Q4EXN1)
     D  p1MIO#       e                     extFld(Q4MIO#)
     D  p1Fl02       e                     extFld(Q4FL02)
     D  p1OdPt       e                     extFld(Q4ODPT)
     D  p1Fl03       e                     extFld(Q4FL03)
     D  p1Spl#       e                     extFld(Q4SPL#)
     D  p1Fl06       e                     extFld(Q4FL06)
     D  p1LnRw       e                     extFld(Q4LNRW)
     D  p1ClCh       e                     extFld(Q4CLCH)
     D  p1NRec       e                     extFld(Q4NREC)
     D  p1Fl04       e                     extFld(Q4FL04)
     D  p1OvLn       e                     extFld(Q4OVLN)
     D  p1Fl05       e                     extFld(Q4FL05)
     D  p1RecF       e                     extFld(Q4RECF)
     D  p1Fl07       e                     extFld(Q4FL07)
     D  p1Lin#       e                     extFld(Q4LIN#)
     D  p1Pag#       e                     extFld(Q4PAG#)
     D p1infdsFL     e ds                  extname(szq4p)
     D  p1FileFL     e                     extFld(Q4FILE)
     D  p1OpenFL     e                     extFld(Q4OPEN)
     D  p1EofFL      e                     extFld(Q4EOF )
     D  p1StCdFL     e                     extFld(Q4STCD)
     D  p1OpCdFL     e                     extFld(Q4OPCD)
     D  p1RtnFL      e                     extFld(Q4RTN )
     D  p1SSeqFL     e                     extFld(Q4SSEQ)
     D  p1RcPrFL     e                     extFld(Q4RCPR)
     D  p1ExN1FL     e                     extFld(Q4EXN1)
     D  p1MIO#FL     e                     extFld(Q4MIO#)
     D  p1Fl02FL     e                     extFld(Q4FL02)
     D  p1OdPtFL     e                     extFld(Q4ODPT)
     D  p1Fl03FL     e                     extFld(Q4FL03)
     D  p1Spl#FL     e                     extFld(Q4SPL#)
     D  p1Fl06FL     e                     extFld(Q4FL06)
     D  p1LnRwFL     e                     extFld(Q4LNRW)
     D  p1ClChFL     e                     extFld(Q4CLCH)
     D  p1NRecFL     e                     extFld(Q4NREC)
     D  p1Fl04FL     e                     extFld(Q4FL04)
     D  p1OvLnFL     e                     extFld(Q4OVLN)
     D  p1Fl05FL     e                     extFld(Q4FL05)
     D  p1RecFFL     e                     extFld(Q4RECF)
     D  p1Fl07FL     e                     extFld(Q4FL07)
     D  p1Lin#FL     e                     extFld(Q4LIN#)
     D  p1Pag#FL     e                     extFld(Q4PAG#)
    D*
     D maxClass        c                   50
     D maxItem1        c                   29
     D maxItem135      c                   50
     D maxItem1u       c                   47
     D maxItem3u       c                   47
     D maxItem4u       c                   47
     D uehd            c                   const('State Unemployment #:')
     D maxItem3c       c                   40
     D maxItem1c       c                   40
     D maxItem4c       c                   32
     D maxItem3D       c                   60
     D maxItem4        c                   60
     D maxItem4BI      c                   45
     D maxClassBI      c                   45
     D maxItem3BI      c                   50
     D e29503          c                   const('WC290503B')
     D e29601          c                   const('WC290601A')
     D e00503          c                   const('WC000503A')
     D e00106          c                   const('WC000106A')
     D e00201          c                   const('WC000201A')
     D e00301          c                   const('WC000301A')
     D e00311          c                   const('WC000311A')
     D e00406          c                   const('WC000406A')
     D e00401          c                   const('WC000401A')
     D e00409          C                   const('WC290409A')
     D e37310          C                   const('WC370310B')
     D E37311          C                   const('WC370310C')
     D lubwv           c                   const('LUB0020199')
     D e04302          c                   const('WC040302A')
     D e00000          c                   const('WC000000A')
     D pn4992          c                   const('PN049902B')
     D xx4001          c                   const('WC040001A')
     D XZFEIH          c                   const('Fein:')
     D XZSICH          c                   const('SIC:')
     D XZNAIH          c                   const('NAICS:')
     D XXADTP          s                   like(a5adtp)
     D X2ADTP          s                   like(a5adtp)
     D isDupLocAddPA   s              1N                                        DupLoc/EntPhysAddres
     D isDupLocAddMA   s              1N                                        DupLoc/EntMailAddres
     D XO1Ln           s             75    Inz(*blanks)
     D XO1ds           ds                  dim(11) Qualified
     D Left                    1     40
     D Right                  41     75
     D Right2                 57     75
     D XO1Indent       s              3    Inz('   ')
     D n               s              2  0 Inz
     D XOSUEH          s             10    Inz(*blanks)
     D XOFEID          s              9    Inz(*blanks)
     D XOFEIH          s              5    Inz(*blanks)
     D XOSICD          s              6    Inz(*blanks)
     D XOSICH          s              9    Inz(*blanks)
     D XONAID          s             10    Inz(*blanks)
     D XONAIH          s             11    Inz(*blanks)
     D xxFD01A         c                   const('Information Page')
     D xxfd01          c                   const('CA INFORMATION PAGE')
     D xxfd02          c                   const('CA SCHEDULE OF NAMED-
     D                                      INSUREDS')
     D xxfd03          c                   const('CA SCHEDULE OF LOCAT-
     D                                     IONS')
     D xxfd04          c                   const('CA SCHEDULE OF FORMS')
     D xxfd05          c                   const('CA SCHEDULE OF OPERA-
     D                                     TIONS')
     D xxFD55          c                   'Item 4-Ext of -
     D                                     Information Page'
     D xxFD56          c                   'Item 3D-Schedule of -
     D                                     Forms & Endorsements'
     D xxFD57          c                   'Item 1-Schedule of -
     D                                     Locations'
     D xxFD58          c                   'Item 1-Schedule of -
     D                                     Named Insureds'
     D xxUW04          c                   const('ADDITIONAL NAMED INSURED -
     D                                     SCHEDULE')
     D xxUW05          c                   const('ADDITIONAL LOCATIONS SCHE-
     D                                     DULE')
     D xxUW01          c                   const('EXTENSION OF INFORMATION -
     D                                     PAGE SCHEDULE OF OPERATIONS')
     D xxUW09          c                   const('ENDORSEMENT SCHEDULE')
     D x1lshd          c                   const('LeaseID:')
     D rstnlk          c                   const('WARSTNLKC')
     D mailhd          c                   const('Mailing Address:')
     D d1dtl           c                   const('SEE SCHEDULE OF -
     D                                     ENDORSEMENTS')
     D d2dtl           c                   const('NO ENDORSEMENTS -
     D                                     ATTACHED TO POLICY')
     D peoext          c                   const('L/C/F')
     D peohd           c                   const('L/C/F N.J. Tax Id No.:')
     D peohdFlex       c                   const(' N.J. Tax Id No.:')
     D xxpeohdFlex     s             25    Inz(*blanks)
     D PRODHD          c                   const('PRODUCER: ')
     D xxtit1          c                   const('Workers Compensation &')
     D xxtit2          c                   const('Employers Liability Policy')
     D xxtit3          c                   const('Information Page')
     D comp3c1         c                   'All states and U.S. territories -
     D                                     except New York, California, mono-
     D                                     polistic states, Puerto Rico,'
     D comp3c2         c                   'the U.S. Virgin Islands, and -
     D                                     states designated in Item 3.A. of -
     D                                     the Information Page.'
     D                 ds
     D  sst                    1     30    dim(15)
     D  shst01                 1      2
     D  shst02                 3      4
     D  shst03                 5      6
     D  shst04                 7      8
     D  shst05                 9     10
     D  shst06                11     12
     D  shst07                13     14
     D  shst08                15     16
     D  shst09                17     18
     D  shst10                19     20
     D  shst11                21     22
     D  shst12                23     24
     D  shst13                25     26
     D  shst14                27     28
     D  shst15                29     30
     D                 ds
     D  sss                    1     15    dim(15)
     D  shss01                 1      1
     D  shss02                 2      2
     D  shss03                 3      3
     D  shss04                 4      4
     D  shss05                 5      5
     D  shss06                 6      6
     D  shss07                 7      7
     D  shss08                 8      8
     D  shss09                 9      9
     D  shss10                10     10
     D  shss11                11     11
     D  shss12                12     12
     D  shss13                13     13
     D  shss14                14     14
     D  shss15                15     15
     D                 ds
     D  ssc                    1     15    dim(15)
     D  shcd01                 1      1
     D  shcd02                 2      2
     D  shcd03                 3      3
     D  shcd04                 4      4
     D  shcd05                 5      5
     D  shcd06                 6      6
     D  shcd07                 7      7
     D  shcd08                 8      8
     D  shcd09                 9      9
     D  shcd10                10     10
     D  shcd11                11     11
     D  shcd12                12     12
     D  shcd13                13     13
     D  shcd14                14     14
     D  shcd15                15     15
     D                 ds
     D  ssd                    1    105  0 dim(15)
     D  shdt01                 1      7  0
     D  shdt02                 8     14  0
     D  shdt03                15     21  0
     D  shdt04                22     28  0
     D  shdt05                29     35  0
     D  shdt06                36     42  0
     D  shdt07                43     49  0
     D  shdt08                50     56  0
     D  shdt09                57     63  0
     D  shdt10                64     70  0
     D  shdt11                71     77  0
     D  shdt12                78     84  0
     D  shdt13                85     91  0
     D  shdt14                92     98  0
     D  shdt15                99    105  0
      *
     D                 ds
     D  sti                    1     90  0 dim(15)
     D  shtm01                 1      6  0
     D  shtm02                 7     12  0
     D  shtm03                13     18  0
     D  shtm04                19     24  0
     D  shtm05                25     30  0
     D  shtm06                31     36  0
     D  shtm07                37     42  0
     D  shtm08                43     48  0
     D  shtm09                49     54  0
     D  shtm10                55     60  0
     D  shtm11                61     66  0
     D  shtm12                67     72  0
     D  shtm13                73     78  0
     D  shtm14                79     84  0
     D  shtm15                85     90  0
     D svt             s              2    dim(15)
     D svs             s              1    dim(15)
     D svc             s              1    dim(15)
     D svd             s              7  0 dim(15)
     D stArr           s              2    dim(15)
     D                 ds
     D  n1name                 1    450
     D  n1nam1               001    050
     D  n1nam2               051    100
     D  n1nam3               101    150
     D  n1nam4               151    200
     D  n1nam5               201    250
     D  n1nam6               251    300
     D  n1nam7               301    350
     D  n1nam8               351    400
     D  n1nam9               401    450
     D*
     D arrst           s              2
     D                                     DIM(52) ASCEND
     D                 ds
     D xxzip                          9  0
     D  xxzip5                        5    overlay(xxzip)
     D  xxzip4                        4    overlay(xxzip:6)
     D                 ds
     D ofzip                          9  0
     D  ofmz1                         5    overlay(ofzip)
     D  ofmz2                         4    overlay(ofzip:6)
    D*
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D                 ds
     D xxasp                          8
     D  x1asp                         3    overlay(xxasp)
     D  xxdot                         1    overlay(xxasp:4)
     D  x2asp                         4    overlay(xxasp:5)
     D                 ds
     D xxasp1                         8
     D  x1asp1                        2    overlay(xxasp1)
     D  xxdot1                        1    overlay(xxasp1:3)
     D  x2asp1                        5    overlay(xxasp1:4)
     D                 ds
     D xxaspe                         7
     D  x1aspe                        2    overlay(xxaspe)
     D  xxdote                        1    overlay(xxaspe:3)
     D  x2aspe                        4    overlay(xxaspe:4)
     D                 ds
     D xxandt                         7  0
     D  xxanyr                        3  0 overlay(xxandt)
     D                 ds
     D xppol                         18
     D  xqfnd                         3    overlay(xppol)
     D  xqemp#                        9    overlay(xppol:4)
     D  xqfyr                         3    overlay(xppol:13)
     D                 ds
     D xpol                          18
     D  xopol                        11    overlay(xpol)
     D   xfnd                         3    overlay(xopol)
     D  xemp#                         9    overlay(xpol:4)
     D   xoemp#                       5    overlay(xemp#)
     D   xofyr                        3    overlay(xemp#:6)
     D  xoblnk                        7    overlay(xpol:12)
     D   xfyr                         3    overlay(xoblnk:2)
     D curpol          ds            18    INZ
     D  curco                         3    overlay(curpol)
     D  curfnd                        3    overlay(curpol:4)
     D  curemp#                       9    overlay(curpol:7)
     D  curfyr                        3    overlay(curpol:16)
      *
     D prvpol          ds            18    INZ
     D  prvco                         3    overlay(prvpol)
     D  prvfnd                        3    overlay(prvpol:4)
     D  prvemp#                       9    overlay(prvpol:7)
     D  prvfyr                        3    overlay(prvpol:16)
     D                 ds
     D qckey                         40
     D  qcco#                         3  0 overlay(qckey)
     D  qcfnd                         3  0 overlay(qckey:4)
     D  qcemp#                        9  0 overlay(qckey:7)
     D  qcdiv                         5  0 overlay(qckey:16)
     D  qcqte                         1    overlay(qckey:40)
     D                 ds
     D  ed                     1     35  0 dim(5) ascend
     D  emefd1               001    007  0
     D  emefd2               008    014  0
     D  emefd3               015    021  0
     D  emefd4               022    028  0
     D  emefd5               029    035  0
     D                 ds
     D  cd                     1     35  0 dim(5) ascend
     D  emcad1               001    007  0
     D  emcad2               008    014  0
     D  emcad3               015    021  0
     D  emcad4               022    028  0
     D  emcad5               029    035  0
     D                 ds
     D  ed3                    1     35  0 dim(5) ascend
     D  c_emefd1             001    007  0
     D  c_emefd2             008    014  0
     D  c_emefd3             015    021  0
     D  c_emefd4             022    028  0
     D  c_emefd5             029    035  0
     D                 ds
     D  cd3                    1     35  0 dim(5) ascend
     D  c_emcad1             001    007  0
     D  c_emcad2             008    014  0
     D  c_emcad3             015    021  0
     D  c_emcad4             022    028  0
     D  c_emcad5             029    035  0
     D                 ds
     D qqpol#                        15
     D  qpfnd                         3    overlay(qqpol#)
     D  qpemp#                        9    overlay(qqpol#:4)
     D  qpfyr                         3    overlay(qqpol#:13)
     D                 ds
     D qqcmd                         80
     D  xxovl                         8    overlay(qqcmd:26)
     D                 ds
     D eneddt                         7  0
     D  xxedyy                        2    overlay(eneddt:2)
     D  xxedmm                        2    overlay(eneddt:4)
     d sttot2          ds                  occurs(52)
     d  t2st                   1      2
     d  t2man                  3     13  2
     d  t2nor                 14     24  2
     D sttots          ds                  occurs(52)
     D tast                           2
     D tas01                         11  2
     D tas02                         11  2
     D tas03                         11  2
     D tas04                         11  2
     D tas05                         11  2
     D tas06                         11  2
     D tas07                         11  2
     D tas08                         11  2
     D tas09                         11  2                                      10
     D tas10                         11  2
     D tas11                         11  2
     D tas12                         11  2
     D tas13                         11  2
     D tas14                         11  2
     D tas15                         11  2
     D tas16                         11  2
     D tas17                         11  2
     D tas18                         11  2
     D x#emd1                         6  0                                      20
     D x#emd2                         6  0
     D x#emd3                         6  0
     D xsexm1                         5  4
     D xsexm2                         5  4
     D xsexm3                         5  4
     D tas19                         11  2
     D tas20                         11  2
     D tas21                         11  2
     D tas22                         11  2
     D xsemd1                         8  0                                      30
     D xsemd2                         8  0
     D xsemd3                         8  0
     D tas23                         11  2
     D xxil1                          2
     D xxil2                          2
     D tas24                         11  2
     D tas25                         11  2
     D xsdiv                          5  0
     D tas26                         11  2
     D tas27                         11  2                                      40
     D tas28                         11  2
     D tms02                         11  2
     D tms06                         11  2
     D tms07                         11  2
     D tms09                         11  2
     D tms14                         11  2
     D tms15                         11  2
     D tms16                         11  2
     D tms18                         11  2
     D tms19                         11  2                                      50
     D tms23                         11  2
     D tas29                         11  2
     D tas30                         11  2
     D tms14b                        11  2
     D tms06b                        11  2
     D tms18b                        11  2
     D tas06b                        11  2
     D tas18b                        11  2
     D  xsexpa                       11  2
     D tas31                         10  2                                      60
     D tas32                         10  2
     D tas33                         10  2
     D tas34                         10  2
     D tas35                         10  2
     D tas36                         10  2
     D xdclas                         5  0
     D xspfdt                         7  0
     D xsptdt                         7  0
     D tasvlc                        11  0
     D tasdtr                        11  0                                      70
     D tas37                         11  2
      *** Beg Add ***** 07/03/25 *****************************
     D tas38                         11  2
     D tms38                         11  2
      *** End Add ***** 07/03/25 *****************************
     D tas39                         11  2
     D tas40                         11  0
     D tas41                         11  0
     D tasacr                        11  0
     D tasexpa                       11  2                                      state exp mod chg
     D tas44                         11  2                                      HCN credit
     D tms44                         11  2                                      HCN credit pre-min
     D tasHMBA                       11  2                                      Habor Min Bal Amt
     D tdco                          15                                         80
     D xsandt                         7  0
     D tas45                         11  2                                      HCN credit
     D xsExMd1Nf                      1
     D xsExMd2Nf                      1
     d xsExMd3Nf                      1
     D tasDisP                        5  2                                      HCN credit
     D tasPrem                       11  2                                      HCN credit
     D tasTerPay                     13  2
     D tasTerPay2                    13  2
     D tasDomPay                     13  2                                      90
     D tasTerPrm                     13  2
     D tasTerPrm2                    13  2
     dtasPolFee                       7  2
     D tasTerRate                     5  3
     D tasDomRate                     5  3
     D xsarp1                         5  4
     D xsarp2                         5  4
     D xsarp3                         5  4
     D xsexp1                        11  2
     D xsexp2                        11  2
     D xsexp3                        11  2
      *** Beg Add *****  07/07/23  *******************************************
     D tms01                         11  2
     D tms03                         11  2
     D tms04                         11  2
     D tms05                         11  2
     D tms08                         11  2
     D tms20                         11  2
     D tms30                         11  2
     D tmsvlc                        11  0
     D tmsacr                        11  0
      *** End Add *****  07/07/23  *******************************************
     D*** begin add ***  09/18/23  **************************************
     D tassf                         11  0
     D*** end add *****  09/18/23  **************************************
     D*** begin add ***  12/04/23  **************************************
     D tasmin                        11  0
     D*** end add *****  12/04/23  **************************************
      *** Beg Add *****  12/05/23  *******************************************
     D tms27                         11  2
     D taspftr                       11  2
     D taspdtr                       11  2
      *** End Add *****  12/05/23  *******************************************
     D*** begin add ***  06/20/24  **************************************
     D tasstpr                       11  0
     D tps02                         11  0
     D tps03                         11  0
     D tps06                         11  0
     D tps06b                        11  0
     D*** begin add ***  12/15/25  **************************************
     D tps07                         11  0
     D*** end add *****  12/15/25  **************************************
     D tps14                         11  0
     D tps14b                        11  0
     D tps15                         11  0
     D tps16                         11  0
     D tps18                         11  0
     D tps18b                        11  0
     D tps19                         11  0
     D tps20                         11  0
     D tps23                         11  0
     D tps29                         11  0
     D tps30                         11  0
     D tps40                         11  0
     D tps41                         11  0
     D tps44                         11  0
     D tpspolfee                     11  0
     D tpshmba                       11  0
     D tpsdtr                        11  0
     D tpsvlc                        11  0
     D tpsacr                        11  0
     D tpsmerit1                     11  0
     D tpsmerit2                     11  0
     D tpsmerit3                     11  0
     D*** end add *****  06/20/24  **************************************
     D*** begin add ***  01/03/25  **************************************
     D xsCXDT                         7  0
     D*** end add *****  01/03/25  **************************************
     D* Create new multi-state datastructure to hold terror/assessment rate change data
     D strtcg          ds                  occurs(52)
     D tcst                           2
     D tcef1                          7  0
     D tcef2                          7  0
     D tcef3                          7  0
     D tca1r1                         5  3
     D tca1r2                         5  3
     D tca1r3                         5  3
     D tca2r1                         5  3
     D tca2r2                         5  3
     D tca2r3                         5  3
     D tca3r1                         5  3
     D tca3r2                         5  3
     D tca3r3                         5  3
     D tca4r1                         5  3
     D tca4r2                         5  3
     D tca4r3                         5  3
     D tca5r1                         5  3
     D tca5r2                         5  3
     D tca5r3                         5  3
     D tca6r1                         5  3
     D tca6r2                         5  3
     D tca6r3                         5  3
     D tca7r1                         5  3
     D tca7r2                         5  3
     D tca7r3                         5  3
     D tctrr1                         5  3
     D tctrr2                         5  3
     D tctrr3                         5  3
     D tcdtr1                         5  3
     D tcdtr2                         5  3
     D tcdtr3                         5  3
     D tca1a1                        11  2
     D tca1a2                        11  2
     D tca1a3                        11  2
     D tca2a1                        11  2
     D tca2a2                        11  2
     D tca2a3                        11  2
     D tca3a1                        11  2
     D tca3a2                        11  2
     D tca3a3                        11  2
     D tca4a1                        11  2
     D tca4a2                        11  2
     D tca4a3                        11  2
     D tca5a1                        11  2
     D tca5a2                        11  2
     D tca5a3                        11  2
     D tca6a1                        11  2
     D tca6a2                        11  2
     D tca6a3                        11  2
     D tca7a1                        11  2
     D tca7a2                        11  2
     D tca7a3                        11  2
     D tctra1                        11  2
     D tctra2                        11  2
     D tctra3                        11  2
     D tcdta1                        11  2
     D tcdta2                        11  2
     D tcdta3                        11  2
     D tca1s1                         5  0
     D tca1s2                         5  0
     D tca1s3                         5  0
     D tca2s1                         5  0
     D tca2s2                         5  0
     D tca2s3                         5  0
     D tca3s1                         5  0
     D tca3s2                         5  0
     D tca3s3                         5  0
     D tca4s1                         5  0
     D tca4s2                         5  0
     D tca4s3                         5  0
     D tca5s1                         5  0
     D tca5s2                         5  0
     D tca5s3                         5  0
     D tca6s1                         5  0
     D tca6s2                         5  0
     D tca6s3                         5  0
     D tca7s1                         5  0
     D tca7s2                         5  0
     D tca7s3                         5  0
     D tca1f1                         5  0
     D tca1f2                         5  0
     D tca1f3                         5  0
     D tca1d1                         5  0
     D tca1d2                         5  0
     D tca1d3                         5  0
     D                 DS                  INZ
     DNXDPPN                          7  0
     D  xxdpp1                        3  0 Overlay(nxdppn)
     D  xxdpp2                        4  0 Overlay(nxdppn:4)
     D*
     D svlda           ds          1024
     D ldend#                         5  0
     D  ldenda                        5    overlay(ldend#)
     D ldmyrr                         9  0
     D ldeedt                         8  0
     D ldfyr                          3  0
     D ldpyrl                         1
     D ldpbcg                         1
     D ldowcg                         1
      * Equals '1' for Add Named Insured midterm endorsement
     D ldAddNamed             30     30
     D ldEndrPrem                     9  0 Overlay(svlda:31)
     D ldOutputFile                  60    Overlay(svlda:191)
     d*
     D                 ds
     D  arr3ast                1    100    dim(50)
     D  pw3ast01               1      2
     D  pw3ast02               3      4
     D  pw3ast03               5      6
     D  pw3ast04               7      8
     D  pw3ast05               9     10
     D  pw3ast06              11     12
     D  pw3ast07              13     14
     D  pw3ast08              15     16
     D  pw3ast09              17     18
     D  pw3ast10              19     20
     D  pw3ast11              21     22
     D  pw3ast12              23     24
     D  pw3ast13              25     26
     D  pw3ast14              27     28
     D  pw3ast15              29     30
     D  pw3ast16              31     32
     D  pw3ast17              33     34
     D  pw3ast18              35     36
     D  pw3ast19              37     38
     D  pw3ast20              39     40
     D  pw3ast21              41     42
     D  pw3ast22              43     44
     D  pw3ast23              45     46
     D  pw3ast24              47     48
     D  pw3ast25              49     50
     D  pw3ast26              51     52
     D  pw3ast27              53     54
     D  pw3ast28              55     56
     D  pw3ast29              57     58
     D  pw3ast30              59     60
     D  pw3ast31              61     62
     D  pw3ast32              63     64
     D  pw3ast33              65     66
     D  pw3ast34              67     68
     D  pw3ast35              69     70
     D  pw3ast36              71     72
     D  pw3ast37              73     74
     D  pw3ast38              75     76
     D  pw3ast39              77     78
     D  pw3ast40              79     80
     D  pw3ast41              81     82
     D  pw3ast42              83     84
     D  pw3ast43              85     86
     D  pw3ast44              87     88
     D  pw3ast45              89     90
     D  pw3ast46              91     92
     D  pw3ast47              93     94
     D  pw3ast48              95     96
     D  pw3ast49              97     98
     D  pw3ast50              99    100
     D*
     D psds          esds                  extname(szq1p)
    D*
     D lo              c                   const('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D up              c                   const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##adrl          s              3  0
     D ##aodr          s              1
     D ##co#           s              3
     D ##data          s             80
     D ##div           s              5
     D ##emp#          s              9
     D ##fedl          s              3  0
     D ##fnd           s              3
     D ##fyr           s              3
     D ##leng          s              3  0
     D ##naml          s              3  0
     D ##nodr          s              1
     D ##odr           s              1
     D ##peo1          s             57
     D ##peo2          s             57
     D ##st            s              2
     D ##pgfl          s              1
     D ##conl          s              3  0
     D ##peol          s              3  0
     D ##dbaf          s              1
     D ##al2           s              3  0
     D ##nl2           s              3  0
     D ##fl2           s              3  0
     D ##pl2           s              3  0
     D ##cl2           s              3  0
     D @@rtcd          s              1
     D adrLn           s              3  0
     D addNameIns      s              1n
     D a1              s              3  0
     D c               s              3  0
     D charge          s             11  2
     D cltid           s              3
     D cmpal           s              1
     D cmpem           s              1
     D d               s              2  0
     D ddtl            s             50
     D dtm             s              1
     D dtmdot          s              1
     D e               s              2  0
     D endEdition      s                   like(endEdt)
     D extPage#        s              5  0
     D extras1         s              1n   inz(*off)
     D f               s              5  0
     D fdays           s              5  0
     D frmNmA          s             13
     D frmNmO          s             13
     D frmNmP          s             13
     D holdne          s                   like(naencd)
     D holdnn          s                   like(nanlkc)
     D holdse          s                   like(svencd)
     D holdsn          s                   like(svnlkc)
     D s2encd          s                   like(svencd)
     D s2elcd          s                   like(naelcd)
     D locencd         s                   like(naencd)
     D locnlkc         s                   like(nanlkc)
     D locencdFed      s                   like(naencd)
     D locnlkcFed      s                   like(nanlkc)
     D locempy         s                   like(naempy)
     D locemlink       s                   like(emlink)
     D i               s              3  0
     D idx             s              3  0
     D ix              s              3  0
     D i2              s              3  0
     D i3              s              3  0
     D item1Prtd       s              1n
     D item1NPrtd      s              1n
     D item3DPrtd      s              1n
     D item4Prtd       s              1n
     D item4Type       s              3  0 inz
     D item1Line#      s              3  0 inz
     D item3DLine#     s              3  0 inz
     D item4Line#      s              3  0 inz
     D kkdiv           s                   like(emdiv)
     D kkelid          s                   like(ldelid)
     D kkfyr           s                   like(elfyr)
     D length          s             15  5
     D len100          s              3  0
     D mv              s              3  0
     D mvad            s              3  0
     D m1              s              3  0
     D m2              s              3  0
     D numFEIN         s                   like(ANFEIN)
     D ovrvar          s              1
     D otherLoc        s              3  0
     D polPeriod1      s            100    inz
     D polPeriod2      s            100    inz
     D polPeriod3      s            100    inz
     D polPeriodU      s            100    inz
     D premAmt         s                   like(xxTAm)
     D premCd          s                   like(premCode)
     D premRt54        s              5  4
     D premRt55        s              5  5
     D printer         s             10
     D printer1        s             10
     D printedAR       s              1n   inz
     D prtAR           s              1n
     D printedTX       s              1n   inz
     D prtTX           s              1n
     D printedKS       s              1n   inz
     D prtKS           s              1n
     D printedOK       s              1n   inz
     D prtOK           s              1n
     D printedOR       s              1n   inz
     D prtOR           s              1n
     D printedPA       s              1n   inz
     D prtPA           s              1n
     D prtDE           s              1n
     D printedDE       s              1n   inz
     D prtMO           s              1n
     D printedMO       s              1n   inz
     D prtMT           s              1n
     D printedMT       s              1n   inz
     D prtCA           s              1n
     D prtFL           s              1n
     D prtNJ           s              1n
     D prt139          s              1n
     D prt025          s              1n
     D currentYear     s              1n
     D liveYear        s              1n
     D ##suffix        s              3a
     D command         s             80a
     D desc20          s             20a
     D month           s              9a
     D toMonth         s              9a
     D day             s              2s 0
     D printPN         s              1n
     D prt132          s              1n
     d unitedWis       s              1n
     d xxPolSt         s              2
     d @@multiSt       s              1n
     D svue#           s                   like(emue#)
     D svsemp          s                   like(xxsemp)
     D svrsk#          s                   like(xxrsk#)
     D xxecnt          s                   like(epecnt)
     D x2ecnt          s                   like(epecnt)
     d amTrust         s              1n
     d xxLastItem      s              1n
     D xxSic           s             20
     D maxItem4am      c                   100
     D maxItem1am      c                   29
     D arrDate         s              8  0 dim(24)
     D arrPrem         s             11  2 dim(24)
     D arrSurc         s             11  2 dim(24)
     D arrFee          s             11  2 dim(24)
     D arrTotal        s             11  2 dim(24)
     d svMM            s              2  0
     d svPodt          s              8  0
     D                 DS
     D  X7PFDT                 1      7  0
     D  YY                     1      3  0
     D  MM                     4      5  0
     D  DD                     6      7  0
     d isOptaComp      s              1n
     D pdays           s              5  0
     D xepror          s                   LIKE(elpror) INZ
     D XXPRTD          s             11  2 INZ
     D pdays3          s              5  0
     D q#apid          s              2
     D q#cdat          s              8
     D q#code          s              2
     D q#comm          s            256
     D q#com2          s             19
     D q#csid          s              2
     D q#dcid          s             12
     D q#dctp          s              5
     D q#difl          s              1
     D q#fl01          s              1
     D q#fl02          s              1
     D q#fold          s             80
     D q#idoc          s             12
     D q#imgf          s              1
     D q#ipgm          s             10
     D q#ipth          s             10
     D q#ique          s             10
     D q#key           s             40
     D q#pgm           s             10
     D q#pidx          s              5
     D q#pnam          s             10
     D q#qpty          s              1
     D q#rmcd          s              5
     D q#sbyl          s              1
     D q#secl          s              3
     D q#seq#          s              5
     D q#user          s             10
     D qqagnm          s             50
     D qqagt#          s              5
     D qqagto          s              1
     D qqamad          s             50
     D qqamcz          s             50
     D qqapad          s             50
     D qqapcz          s             50
     D qqare1          s              3
     D qqb1            s             12
     D qqb2            s             80
     D qqco#           s              3
     D qqcpy           s              2
     D qqdart          s              1
     D qqdays          s              5  0
     D qqdcid          s             12
     D qqdiv           s              5
     D qqdmbr          s             10
     D qqdoc           s             12
     D qqemp#          s              9
     D qqerr           s              1
     D qqfina          s              1
     D qqflr           s             80
     D qqfnd           s              3
     D qqfold          s             80
     D qqfrom          s              8  0
     D qqfyr           s              3
     D qqhard          s              1
     D qqlen           s             15  5
     D qqmcty          s             30
     D qqmst           s              2
     D qqmzip          s              9
     D qqpcty          s             30
     D qqpdf           s              1
     D qqpflr          s             80
     D qqphn1          s              7
     D qqpre           s              2
     D qqPrinter       s                   like(printer)
     D qqPrinter1      s                   like(printer)
     D qqprnm          s             50
     D qqprod          s              5
     D qqPrtf          s                   like(printer)
     D qqPrtf1         s                   like(printer)
     D qqpst           s              2
     D qqpzip          s              9
     D qqque           s             10
     D qqreis          s              1
     D qqrnb           s              1
     D qqrtcd          s              1
     D qqsdoc          s                   like(f2dcid)
     D qqspl#          s              4
     D qqstep          s              1
     D qqsub#          s              5
     D qqterm          s              1
     D qqto            s              8  0
     D q2fold          s             40
     D r               s              2  0
     D s               s              2  0
     D subst           s              3  0 inz(25)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D strpdt          s                   like(emdt)
     D svandt          s                   like(emdt)
     D svdiv           s              5  0
     D svelcd          s              5  0
     D svemd1          s                   like(xxemd1)
     D svemd2          s                   like(xxemd2)
     D svemd3          s                   like(xxemd3)
     D svencd          s              3  0
     D svexm1          s                   like(xxexm1)
     D svexm2          s                   like(xxexm2)
     D svexm3          s                   like(xxexm3)
     D xxdisp          s                   like(eldisp)
     D svfdnm          s             40
     D svfdn2          s             60
     D svform          s             15
     D svin26          s              1
     D svin60          s                   like(*in60)
     D svin91          s              1
     D svin92          s              1
     D svin93          s              1
     D svnaob          s             40
     D svnlkc          s                   like(nanlkc)
     D svrrn           s                   like(emrrn)
      **** Beg Chg **** 06/17/24 *******************************************
     D***svrt            s              5  2
     D svrt            s              8S 3
      **** End Chg **** 06/17/24 *******************************************
     D svst            s              2
     D svclas          s                   like(epclas)
     D svspec          s                   like(epspec)
     D svefdt          s                   like(epefdt)
     D svexdt          s                   like(epefdt)
     D svpst           s                   like(eppst)
     D svfrav          s                   like(epfrav)
     D svtoav          s                   like(eptoav)
     D svarat          s                   like(eparat)
     D svmad1          s                   like(emmad1)
     D svmad2          s                   like(emmad2)
     D svmcty          s                   like(emmcty)
     D svmst           s                   like(emmst)
     D svmzip          s                   like(emmzip)
     D svnona          s                   like(emnonm)
     D svnm1           s                   like(naname)
     D svnm3           s                   like(nanam3)
     D svnm4           s                   like(nanam4)
     D t               s              3  0
     D tt              s              2  0
     D turn            s              1  0
     D useovl          s              1
     D usePRTF         s              1n
     D w               s              2  0
     D writfd          s              1
     D writma          s              1
     D writpa          s              1
     D x               s              3  0
     D xafein          s              9
     D xamad1          s                   like(almad1)
     D xamad2          s                   like(almad2)
     D xamcty          s                   like(almcty)
     D xamst           s                   like(almst)
     D xapad1          s                   like(alpad1)
     D xapad2          s                   like(alpad2)
     D xapcty          s                   like(alpcty)
     D xapst           s                   like(alpst)
     D xemad1          s                   like(emmad1)
     D xemad2          s                   like(emmad2)
     D xemcty          s                   like(emmcty)
     D xemst           s                   like(emmst)
     D xepad1          s                   like(empad1)
     D xepad2          s                   like(empad2)
     D xepcty          s                   like(empcty)
     D xeprem          s             11  2
     D xepst           s                   like(empst)
     D xest            s              2
     D xfencd          s                   like(naencd)
     D*** begin add ***  01/12/22  ******************************************
     D xfnlkc          s                   like(nanlkc)
     D*** end add *****  01/12/22  ******************************************
     D xfname          s              1
     D xl              s              1  0
     D xlmin1          s             11  2
     D xlmin2          s             11  2
     D xlpct           s              5  3
     D xnarat          s              9  4
     D xna100          s            100
     D xnempr          s             11  2
     D xnfein          s                   like(nafein)
     D xnlegn          s                   like(nalegn)
     D xnmad1          s                   like(namad1)
     D xnmad2          s                   like(namad2)
     D xnmcty          s                   like(namcty)
     D xnmst           s                   like(namst)
     D xnpror          s              9  6
     D xnsav           s            100
     D xofein          s                   like(emfein)
     D xqfrom          s              8  0
     D xqto            s              8  0
     D xsexco          s             11  2
     D xskey           s              2
     D xsname          s              1
     D xsspl#          s              4  0
     D xxacc           s              9  0
     D xxacsz          s             34
     d xxaCity         s             40
     d xxaSt           s              2
     d xxaZip          s              9s 0
     d xxfCity         s             40
     d xxfSt2          s              2
     d xxfZip          s              9s 0
     D xxadd1          s              1
     D xxadld          s                   like(epadld)
     D xxagad          s             34
     D xxagg           s              9  0
     D xxagnm          s             34
     D xxAgNmFull      s                   like(prodName)
     D xxagt#          s              5  0
     D xxahdr          s             10
     D xxaloc          s              1
     d idxStart        s              2  0
     d isPrtInstSched  s               n
     d instSchdFile    s             60a
     d instSchdForm    s             18a
     d*** begin add ***  01/09/24  **************************************
     d instSchd1       s             18a
     d instSchd2       s             18a
     d instSchd3       s             18a
     d*** end add *****  01/09/24  **************************************
     d instSchdDate    s              7  0
     d instSchdDesc    s            100a
     d instSchdSeq     s              3  0
     d index           s              2  0
     d insAmount       s             13  2 dim(55) inz(0)
     d insCount        s              2  0
     d insDueDt        s              8  0 dim(55) inz(0)
     d insFrDt         s              8  0 dim(55) inz(0)
     d insPct          s              5  2 dim(55) inz(0)
     d insToDt         s              8  0 dim(55) inz(0)
     d savXtDCid       s                   Like(XTDcid)
     d savXTPath       s                   Like(XTPath)
     d savXTLang       s                   Like(XTLang)
     d*** begin add ***  03/04/24  **************************************
     d xxExRt          s              1
     d*** End   add ***  03/04/24  **************************************
     d                 ds
     d  ab                     1    108  8
     d                                     dim(12) inz(0)
     d  BSab01                 1      9  8
     d  BSab02                10     18  8
     d  BSab03                19     27  8
     d  BSab04                28     36  8
     d  BSab05                37     45  8
     d  BSab06                46     54  8
     d  BSab07                55     63  8
     d  BSab08                64     72  8
     d  BSab09                73     81  8
     d  BSab10                82     90  8
     d  BSab11                91     99  8
     d  BSab12               100    108  8
     D xxanam          s              1
     D xxaspx          s              9
     D xxbtyp          s             20
     D xxchg           s              1
     D xxclas          s                   like(prclas)
     D xxco#           s              3  0
     D xxcode          s              5  0
     D xxct01          s                   like(fncgfl)
     D xxdate          s              8  0
     D xxdba           s              3
     D xxdcid          s                   like(cmdcid)
     D xxddcd          s                   like(eldcde)
     D xxdesc          s                   like(mudesc)
     D svbtyp          s                   like(emnaob)
     D xxbdes          s                   like(emnaob)
     D x1              s              1
     D x2              s              1
     D x3              s              1
     D x4              s              1
     D x5              s              1
     D xxpa2           s              1
     D xxfrst          s              1
     D xxmc            s              3  0
     D xxdis           s              9  0
     D xxdisc          s                   like(epdisc)
     D xxdiv           s                   like(emdiv)
     D xxdv3           s              3
     D xxead1          s             40
     D xxead2          s             40
     D xxeapr          s                   like(epeapr)
     D xxecsz          s             40
     D xxeedt          s                   like(aldldt)
     D xxefdt          s                   like(elptdt)
     D xxf4dt          s                   like(elpfdt)
     D xxf4ky          s                   like(f4key)
     D x1efdt          s                   like(elptdt)
     D x3efdt          s                   like(elptdt)
     D xxefl1          s              1
     D xxefl2          s              1
     D xxefl3          s              1
     D xxelcd          s              5  0
     D xxemd1          s              8  0
     D xxemd2          s              8  0
     D xxemd3          s              8  0
     D xxemn1          s             50
     D xxemn2          s             50
     D xxemn3          s             50
     D xxemn4          s             50
     D xxemn5          s             50
     D xxemn6          s             50
     D xxemn7          s             40
     D xxemn8          s             40
     D xxemn9          s             40
     D xxamn1          s             60
     D xxamn2          s             60
     D xxamn3          s             60
     D xxamn4          s             60
     D xxamn5          s             60
     D xxamn6          s             60
     D xxamn7          s             40
     D xxamn8          s             40
     D xxamn9          s             40
     D xxemp#          s                   like(ememp#)
     D xxempr          s                   like(epempr)
     D xxentp          s                   like(epentp)
     D xxenam          s             40
     D xxencd          s                   like(naencd)
     D xxend#          s                   like(muend#)
     D xxenm2          s             35
     D xxenm3          s             35
     D xxEnSt          s                   like(MUENST)
     D xxerpt          s              1
     D xxestp          s             11  2
     D xxexco          s                   like(ecexco)
     D xxexm1          s                   like(elexm1)
     D xxexm2          s                   like(elexm2)
     D xxexm3          s                   like(elexm3)
     D xxnoexm1        s                   like(ElNoExm1)
     D*** Begin Add *** 07/30/23 *******************************************
     D xxnoexm2        s                   like(ElNoExm2)
     D xxnoexm3        s                   like(ElNoExm3)
     D*** End   Add *** 07/30/23 *******************************************
     D xsnoexm1        s                   like(ElNoExm1)
     D*** Begin Add *** 07/30/23 *******************************************
     D xsnoexm2        s                   like(ElNoExm2)
     D xsnoexm3        s                   like(ElNoExm3)
     D*** End   Add *** 07/30/23 *******************************************
     D xxext           s              5
     D xxfact          s              5  4
     D xxfcsz          s             40
     D xxfdn1          s             50
     D xxfdn2          s             50
     D xxfein          s                   like(emfein)
     D x1fein          s                   like(emfein)
     D svfein          s                   like(emfein)
     D x1emp#          s                   like(ememp#)
     D x1div           s                   like(emdiv)
     D xxfnd           s              3  0
     D xxfold          s                   like(cmfold)
     D xxform          s                   like(muform)
     D xxform1         s                   like(muform)
     D xxform2         s                   like(muform)
     D xxform3         s                   like(muform)
     D xxform4         s                   like(muform)
     D xxform5         s                   like(muform)
     D FORMB           S              1    DIM(15)
     D FORMA           S              1    DIM(15)
     D xxfrav          s              6  0
     D xxfrid          s                   like(muform)
     D xxfyr           s              3  0
     D xxf100          s            100
     D xxf2            s              2
     D xxhard          s              1
     D xxhdg           s             50
     D xxHeading       s             50
     D xxSign          s              1
     D xxPercent       s              9  6
     D xxPercentF      s              9  6
     D xxFactor        s              9  6
     D xxStatCode      s              5  0
     D xxhold          s                   like(xxenam)
     D xxhst           s                   like(empst)
     D xxilcd          s             10
     D xxilef          s                   like(elilef)
     D xxinc           s              4
     D xxkey           s                   like(gtkey)
     D xxlday          s              5  0
     D xxline          s              6  0
     D xxlmef          s                   like(elilef)
     D xxlmex          s                   like(elilex)
     D xxlpdt          s                   like(emdt)
     D xxmin           s              9  0
     D xxminf          s              1
     D xxminp          s             11  0
     D xxmin1          s                   like(lmmine)
     D xxmin2          s                   like(lmmine)
     D xxmnpr          s                   like(elenmp)
     D xxexcn          s                   like(prmnpr)
     D xomin1          s                   like(elenmp)
     D xomin2          s                   like(elenmp)
     D xxmpec          s             11  2
     D xxmpst          s              2  0
     D xxmst           s              2  0
     D xxmths          s              3  1
     D xxmxsp          s              5  0
     D xxnam1          s             50
     D xxnam2          s             50
     D xsnam1          s             50
     D xsnam2          s             50
     D xsfdn1          s             50
     D xsfdn2          s             50
     D xxnegf          s              1
     D xxnj            s              2
     D xxnjde          s              1
     D xxnjst          s              1
     D xxflst          s              1
     D xxCAst          s              1
     d xnemsn          s              1    inz
     d xxstcd          s              5  0
     d xtpred          s                   like(epempr)
     d xxFlRtAdj       s                   like(epempr)
     D xcILRate        c                   const('IL Flat rate adjustment')
     D xxnlkc          s                   like(nanlkc)
     D xxnm10          s             10
     D xxn100          s            100
     D xxoad1          s             40
     D xxoad2          s             40
     D xxoccd          s              1
     D xxNrat          s              1
     D xxocsz          s             40
     D xxodes          s             50
     D xxofad          s             40
     D xxofad100       s            100
     D                 ds
     D  wka                    1    198  2 dim(18)
     d  q1am1                  1    198
     D xxofif          s              1
     D xxonam          s             56
     D xxonm2          s             56
     D xxonm1          s             56
     D xxonm3          s             56
     D xxnm56          s             56
     D xxordr          s              1
     D xxowcg          s              1
     D xxPage          s              3  0
     D xxpbcg          s              1
     D xxpday          s              5  0
     D xxpdf           s              1
     D xxpdm           s              1
     D xxpdpr          s             11  2
     D xxinst          s             11  2
     D xxdep           s             10  2
     D xxbefp          s             11  2
     D xxothr          s             11  2
     D xxetxt          s             15
     D xxpfdt          s              8  0
     D xxpos           s              3  0
     D xxppc3          s             60
     D xxpp3c          s             60
     D xxprag          s             35
     D xxprem          s             11  2
     D xxprin          s              1
     D xxprm           s             11  2
     D xxprnm          s             50
     D xxprod          s              5  0
     D xxpror          s             11  2
     D xxpst           s                   like(empst)
     D xxptdt          s              8  0
     D xxRisk          s                   like(EMSEMP)
     D xxDrsk          s                   like(NHDRSK)
     D xxrmrk          s              5
     D xxrnb           s              1
     D xxrpt#          s              1
     D xxrsk#          s              9
     D xxrt            s              9  6
     D xxrtcd          s                   like(elrtcd)
     D xxsave          s             11  0
     D xxspec          s              1
     D xxspl#          s              4  0
     D xxst            s                   like(ddst)
     D xxste           s              2
     D xxsyk           s              2
     D xxtam           s             11  2
     D xx935           s             11  2
     D xx936           s             11  2
     D xxtbid          s                   like(gttbid)
     D xxtmpr          s                   like(epempr)
     D xxtoav          s              6  0
     D xxtxid          s                   like(emsemp)
     D x1txid          s                   like(emsemp)
     D x3txid          s                   like(emsemp)
     D x4txid          s             35
     D xxue#           s                   like(emue#)
     D svtxid          s                   like(emsemp)
     D xxtype          s              1
     D xstype          s              1
     D svcode          s              1
     D xxunid          s             12  0
     D xxwh54          s              5  4
     D xxwk10          s             11  0
     D xxwk54          s              5  4
     D xxwo54          s              5  4
     D xxwrk8          s              8
     D xxwvmn          s             11  2
     D xx1a            s              1
     D xx1stc          s              5
     D xx1stp          s              7  4
     D xx157           s              1
     D xx157d          s             11  2
     D xzst            s              2
     D x1andt          s              7  0
     D x1clas          s                   like(ddclas)
     D x1from          s                   like(emdt)
     D x1draw          s              7
     D x1hold          s              4
     D x1name          s             57
     D x1p             s              1
     D x1save          s              4
     D x1to            s                   like(emdt)
     D x2acc           s                   like(xxacc)
     D x2agg           s                   like(xxagg)
     D x2clas          s                   like(ddclas)
     D x2dis           s                   like(xxdis)
     D x2div           s                   like(eldiv)
     D x2eapr          s                   like(epeapr)
     D x2efdt          s                   like(epefdt)
     D x2end#          s                   like(muend#)
     D x2name          s             57
     D x2oad1          s             40
     D x2oad2          s             40
     D x2ocsz          s             40
     D x2onam          s             40
     D x2onm2          s             40
     D x2p             s              2
     D x2wk54          s              5  4
     D x3p             s              3
     D x3clas          s                   like(epclas)
     D x3TDF           s                   like(EPtdf)
     d x1eapr          s             11  0
     D svempr          s                   like(epempr)
     D XHPROD          s             15    INZ
     D x5fein          s             15
     D x7ddef          s                   like(el2def)
     D x7fein          s             15
     D x8i2ef          s              8  0
     D y               s              2  0
     D z               s              3  0
     D zzemp#          s                   like(ememp#)
     D sysDt           s               d
     D sysDt7          s              7  0
     D sysDt8          s              8  0
     D xxtxpc          s                   like(elprtd)
     D QQDC22          S             22    INZ
     D qqiflr          s             80    INZ
     D yyemp#          s              9    INZ
     D yyfyr           s              3    INZ
     D @ENV            S              1    INZ
     D xxocty          S             25
     D xxost           S              2
     D xxozip          S              9  0
     D x2octy          S             25
     D x2ost           S              2
     D x01sid          S              1
     D x01sds          S             40
     D x0feid          S              9  0
     D x0sue#          S             20
     D xxxlnk          S              3  0
     D xxxlkc          S              2  0
     D cnt             S              2  0
     D cnt2            S              2  0
     D cnt3            S              2  0
     D kkfnd           S                   like(emfnd)
     D zzrt            S              7  6
     D heading         s               n
     D xxnaic          s              1    INZ
     d x#RRN           s              9  0
     d xxPDMD          s              7  0
     d xxPDMT          s              6  0
     d isPremChg       s              1n
     d xxExMd1Nf       s              1    inz
     d xxExMd2Nf       s              1    inz
     d xxExMd3Nf       s              1    inz
     d xxStRptCxDt     s              7  0
     D*
     D SVpeof          s              1a   inz

     d usrdat          ds            20
     d  usrID                        10
     d  runDate                      10

     D                 ds
     D  hdg1                   1    770
     D                                     DIM(14)
     D  zzhead1a               1     55
     D  zzhead2a              56    110
     D  zzhead3a             111    165
     D  zzhead4a             166    220
     D  zzhead5a             221    275
     D  zzhead6a             276    330
     D  zzhead7a             331    385
     D  zzhead8a             386    440
     D  zzhead9a             441    495
     D  zzhead10a            496    550
     D  zzhead11a            551    605
     D  zzhead12a            606    660
     D  zzhead13a            661    715
     D  zzhead14a            716    770
     D*
     D                 DS
     D  hdg2                   1    770
     D                                     DIM(14)
     D  zzhead1b               1     55
     D  zzhead2b              56    110
     D  zzhead3b             111    165
     D  zzhead4b             166    220
     D  zzhead5b             221    275
     D  zzhead6b             276    330
     D  zzhead7b             331    385
     D  zzhead8b             386    440
     D  zzhead9b             441    495
     D  zzhead10b            496    550
     D  zzhead11b            551    605
     D  zzhead12b            606    660
     D  zzhead13b            661    715
     D  zzhead14b            716    770
     D*
     D                 DS
     D  shdg1                  1    700
     D                                     DIM(14)
     D  zzsbhd1a               1     50
     D  zzsbhd2a              51    100
     D  zzsbhd3a             101    150
     D  zzsbhd4a             151    200
     D  zzsbhd5a             201    250
     D  zzsbhd6a             251    300
     D  zzsbhd7a             301    350
     D  zzsbhd8a             351    400
     D  zzsbhd9a             401    450
     D  zzsbhd10a            451    500
     D  zzsbhd11a            501    550
     D  zzsbhd12a            551    600
     D  zzsbhd13a            601    650
     D  zzsbhd14a            651    700
     D*
     D                 DS
     D  shdg2                  1    700
     D                                     DIM(14)
     D  zzsbhd1b               1     50
     D  zzsbhd2b              51    100
     D  zzsbhd3b             101    150
     D  zzsbhd4b             151    200
     D  zzsbhd5b             201    250
     D  zzsbhd6b             251    300
     D  zzsbhd7b             301    350
     D  zzsbhd8b             351    400
     D  zzsbhd9b             401    450
     D  zzsbhd10b            451    500
     D  zzsbhd11b            501    550
     D  zzsbhd12b            551    600
     D  zzsbhd13b            601    650
     D  zzsbhd14b            651    700
     D*
     D                 DS
     D  nameTitle              1    714
     D                                     DIM(14)
     D  zznmtt1                1     51
     D  zznmtt2               52    102
     D  zznmtt3              103    153
     D  zznmtt4              154    204
     D  zznmtt5              205    255
     D  zznmtt6
     D  zznmtt7              307    357
     D  zznmtt8              358    408
     D  zznmtt9              409    459
     D  zznmtt10             460    510
     D  zznmtt11             511    561
     D  zznmtt12             562    612
     D  zznmtt13             613    663
     D  zznmtt14             664    714
     D*
     D                 DS
     D  extension              1    714
     D                                     DIM(14)
     D  zzext1                 1     51
     D  zzext2                52    102
     D  zzext3               103    153
     D  zzext4               154    204
     D  zzext5               205    255
     D  zzext6               256    306
     D  zzext7               307    357
     D  zzext8               358    408
     D  zzext9               409    459
     D  zzext10              460    510
     D  zzext11              511    561
     D  zzext12              562    612
     D  zzext13              613    663
     D  zzext14              664    714
     D*
     D                 DS
     D  classCod               1     35s 0
     D                                     DIM(7)
     D  code1                  1      5s 0
     D  code2                  6     10s 0
     D  code3                 11     15s 0
     D  code4                 16     20s 0
     D  code5                 21     25s 0
     D  code6                 26     30s 0
     D  code7                 31     35s 0
     D*
     D                 DS
     D  classDsc               1    175
     D                                     DIM(7)
     D  zzcls1                 1     25
     D  zzcls2                26     50
     D  zzcls3                51     75
     D  zzcls4                76    100
     D  zzcls5               101    125
     D  zzcls6               126    150
     D  zzcls7               151    175
     D                 DS
     D  classAmtA              1    105
     D                                     DIM(7)
     D  zzamt1                 1     15
     D  zzamt2                16     30
     D  zzamt3                31     45
     D  zzamt4                46     60
     D  zzamt5                61     75
     D  zzamt6                76     90
     D  zzamt7                91    105
     D                 DS
     D  classAmtN              1     63s 0
     D                                     DIM(7)
     D  amount1                1      9s 0
     D  amount2               10     18s 0
     D  amount3               19     27s 0
     D  amount4               28     36s 0
     D  amount5               37     45s 0
     D  amount6               46     54s 0
     D  amount7               55     63s 0
     D                 DS
     D  classRat               1     56
     D                                     DIM(7)
     D  zzrat1                 1      8
     D  zzrat2                 9     16
     D  zzrat3                17     24
     D  zzrat4                25     32
     D  zzrat5                33     40
     D  zzrat6                41     48
     D  zzrat7                49     56
     D                 ds
     D x2ozip                         9  0 INZ
     D  x2ozip5                       5    overlay(x2ozip)
     D  x2ozip4                       4    overlay(x2ozip:6)
     d bancInsure      s               n
     d a#              s              2  0
     d vertLinePos     s              5  3
     d horzLinePos     s              5  3
     d pos#            s              3  0
     d substr#         s              3  0
     d #OfAddLines     c                   9
     d biMailHd        c                   'Mailing Address:'
     d verPos          ds            45                                         address position
     d vPos                           5  3 dim(#OfAddLines) overlay(verPos)
     d av1                                 overlay(verPos : 1)
     d av2                                 overlay(verPos : 6)
     d av3                                 overlay(verPos : 11)
     d av4                                 overlay(verPos : 16)
     d av5                                 overlay(verPos : 21)
     d av6                                 overlay(verPos : 26)
     d av7                                 overlay(verPos : 31)
     d av8                                 overlay(verPos : 36)
     d av9                                 overlay(verPos : 41)
     d horPos          ds            45                                         address position
     d hPos                           5  3 dim(#OfAddLines) overlay(horPos)
     d ah1                                 overlay(horPos : 1)
     d ah2                                 overlay(horPos : 6)
     d ah3                                 overlay(horPos : 11)
     d ah4                                 overlay(horPos : 16)
     d ah5                                 overlay(horPos : 21)
     d ah6                                 overlay(horPos : 26)
     d ah7                                 overlay(horPos : 31)
     d ah8                                 overlay(horPos : 36)
     d ah9                                 overlay(horPos : 41)
     D yyefdt          s                   like(emefd1)
     D yycxdt          s                   like(emcad1)
     D yysydt          s                   like(emcad1)
      * counters
     d y1              s              3  0
     d*** Beg Add *** 07/13/22 ************************************************
     d intSeq#         s              9  0
     d*** End Add *** 07/13/22 ************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     Daggregate        pr              n
     D company                             like(elco#)
     D group                               like(elfnd)
     D employer                            like(elemp#)
     D division                            like(eldiv)
     D year                                like(elfyr)

     DGetAddress       pr
     DsetNmAddr        pr
     d updatWXNAP      pr
     d  typeAddress                   1    const                                Mailing or Physical
     d updatWXA5L2     pr
     d  typeAddress                   1    const                                Mailing or Physical

     DGetCityStZip     pr           100
     D inCity                              like(FNPCTY)
     D inSt                                like(FNPST)
     D inZipCode                           like(FNPZIP)

     DGetPhone#        pr            13
     D inArea                         3  0
     D inPhone#                       7  0
     D inType                         1    options(*nopass)

     DGetZipCode       pr            10
     D inZipCode                      9  0

     DGetPEOFein       pr             9  0

     DObjExists        pr             1n
     D object                        10    const
     D objType                        7    const
     d getLossCost     pr             5  4
     d  class                              like(EPCLAS)
     d  effDt                              like(EPEFDT) const
     d getRateDate     pr             7  0
     d  effDt                              like(EPEFDT)
     d getStateName    pr            40
     d  stateAbbrev                        like(xxst)
     d getAltEndNm     pr                  extpgm('WA722R')
     d  input                       900
     d  output                     1750
     d getPEOTypB      pr            20
     d getPEOProd      pr            60
     d getAgtInfo      pr                  extpgm('WA002R')
     d  co#                                like(EACO#)  const
     d  fnd                                like(EAFND)  const
     d  emp#                               like(EAEMP#) const
     d  div                                like(EADIV)  const
     d  fyr                                like(EAFYR)  const
     d  agt#                          5  0
     d  agt_name                     60
     d  prod                          5  0
     d  prod_nm                      60
     d  area_cd                       3  0
     d  phone#                        7  0
     d  plan                          1
     d  asOfDate                      8  0
     D @2nper          C                   '2nd Period'
     DQQFOUND          S              1N
     DQQMPAYR          S              7  5
     DQQMRATE          S              7  5
     DQQRPTCLS         S              5  0
     D ApplyRateSplit  pr              n
     D inState                        2a
     D inDate                         7  0
     D outstcd                        5  0 options(*nopass)
     d FClassTotal     pr            11  2
     d SrPrmChg        pr             1n
     D SRCVTDAT        PR                  ExtPgm('SRCVTDAT')
     D  srdti                         8  0 const
     D  srffmt                        8    const
     D  srtfmt                        8    const
     D  srdto                        10
     D  srerr                         1
     d xxNcciStCode    s              2
     d GetNcciStCode   pr             2
     d  physState                     2
     d  mailState                     2
     D ReseqXmlOrder   pr
     D  XmlKey                       60
     D  XmlCo#                        3  0
     D  XmlFnd                        3  0
     dGetInvoiceDates  pr
     d WriteArInst     pr
     d  fromDate                      8  0 Const
     d  toDate                        8  0 Const
     d  index                         2  0 Const
     d*** begin add ***  08/15/23  *********************************************
     D SendEscMsg      pr                  extpgm('QMHSNDPM')
     D   MsgID                        7    const
     D   MsgFile                     20    const
     D   MsgDta                   32000    const options(*varsize)
     D   MsgDtaLen                   10i 0 const
     D   MsgType                     10    const
     D   MsgQ                        10    const
     D   MsgQNbr                     10i 0 const
     D   MsgKey                       4
     D   ErrorDS                     16

     D ErrorDS         ds            16
     D   BytesProv                   10i 0 inz(16)
     D   BytesAvail                  10i 0
     D   ExceptionID                  7

     D MsgDta          s          32000    varying inz(' ')
     D MsgKey          s              4
     d*** end add *****  08/15/23  *********************************************
