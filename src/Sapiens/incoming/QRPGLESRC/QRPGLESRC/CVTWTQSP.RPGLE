     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H bnddir('SPIDATE')
    H*********************************************************************
    H/TITLE CVTWTQSP - Convert data for WTQSP (load records)
    H*********************************************************************
    H* Created: 05/28/09    C:33    Project: 133WGV                      *
    H* Rewrite: 11/05/12    C:33    Project: Transform                   *
    H*********************************************************************
     H* Revised:                                                          *
     H*                                                                   *
     H* 08/31/23  C:300 Support: 113762 (LUBA)                            *
     H*                 Quotes are locking after status changed to Waiting*
     H*                 So, removing 'L' from locking flag of these records
     H*                 I- Waiting                                        *
     H*                 i- Renewal Waiting                                *
     H*                 p- Renewal Pending                                *
     H*                 y- Renewal Agent Review                           *
     H*                                                                   *
     H* 08/26/22  C:62  Project: 129WXI                                   *
     H*                 Add new quote statuses for CoveRisk               *
     H*                                                                   *
     H* 06/22/20  C:33  Project: SY420                                    *
     H*                 Add locking flags                                 *
     H*                                                                   *
     H* 12/11/17  C:33  Project: 133WQI (51614)                           *
     H*                 Adjust sort order                                 *
     H*                                                                   *
     H* 11/22/17  C:62  Project: 133WQI                                   *
     H*                 Add two new status codes                          *
     H*                                                                   *
     H* 10/20/16  C:33  Support: 43775                                    *
     H*                 Different descriptions for 133                    *
    H*********************************************************************
     Fsmcop     if   e           k disk    rename(pco01:pco0p)
     Fwtqsp     uf a e           k disk    rename(pqs01:pqs0p)
 PA  Fwtqsl1    uf   e           k disk

     D*** Begin Chg *** 08/26/22 **************************************
     D*#ofTypes        c                   30
     D #ofTypes        c                   40
     D*** End   Chg *** 08/26/22 **************************************
     D tp              s              1    dim(#ofTypes) ctdata perrcd(1)
     D***  Begin Chg  ***  06/22/20  ***************************
     D**tpd*****       s             82    dim(#ofTypes) alt(tp)
     D  tpd            s             83    dim(#ofTypes) alt(tp)
     D***  End   Chg  ***  06/22/20  ***************************

     D #ofTypes133     c                   2
     D tp133           s              1    dim(#ofTypes133) ctdata perrcd(1)
     D***  Begin Chg  ***  06/22/20  ***************************
     D**tpd133**       s             82    dim(#ofTypes133) alt(tp133)
     D  tpd133         s             83    dim(#ofTypes133) alt(tp133)
     D***  End   Chg  ***  06/22/20  ***************************
     D*** Begin Add *** 08/26/22 *******************************************
     D #ofTypes129     c                   2
     D tp129           s              1    dim(#ofTypes129) ctdata perrcd(1)
     D  tpd129         s             83    dim(#ofTypes129) alt(tp129)
     D*** End   Add *** 08/26/22 *******************************************

     DtypeInfo         ds
     D qte                                 like(QSqte)
     D desc                                like(QSdesc)
     D used                                like(QSused)
     D aarh                                like(QSaARH)
     D aaro                           3s 0
     D aars                           3s 0
     D renew                               like(QSrenew)
     D clientID                       3
     D***  Begin Add  ***  06/22/20  ***************************
     D locking                        1
     D***  End   Add  ***  06/22/20  ***************************

     DcltID            s              3
     D*** Begin Add *** 08/26/22 *******************************************
     Di129             s              3  0
     D*** End   Add *** 08/26/22 *******************************************
     Di133             s              3  0
     Di1               s              3  0
     DxxKey            s                   like(QSkey)
     Dxxqte            s                   like(QSqte)
     DsysTime          s                   like(QStime)
     DsysDate          s                   like(QSdt)

     D psds          esds                  extname(szq1p)

     D/copy sPrDate
     D/copy sPrRunCmd

     DDeleteCode       pr
     D code                                like(QSkey) const

     DRtvCltID         pr                  extPgm('RTVCLTID')
     D cltID                          3

      /Free
       sysTime = TimeTo6(SystemTime);
       sysDate = DateTo7(SystemDate);
       RtvCltID(cltID);

       //Callp RunCmd('CLRPFM FILE(WTQSP)':25);

       Dou %eof(smcop);
          Read smcop;
          If not %eof(smcop);
             Exsr Process;
          Endif;
       Enddo;

       *inlr = *on;

       //==================================================================
       // Process each company
       //==================================================================
       Begsr Process;

       Delete (COco#:'F') wtqsl1;
       Delete (COco#:'G') wtqsl1;
       //DeleteCode('F');
       //DeleteCode('G');

       For i1 = 1 to #ofTypes;
          Chain (COco#:tp(i1)) pqs01;
          typeInfo = tpd(i1);
          If clientID <> ' ' And clientID <> cltID;
             Iter;
          EndIf;
          Select;
          //*** Begin Add *** 08/26/22 ******************************* *****
          When cltID = '129';
             i129 = %lookup(tp(i1):tp129);
             If i129 <> *zero;
                typeInfo = tpd129(i129);
             Endif;
          //*** End   Add *** 08/26/22 ******************************* *****
          When cltID = '133';
             i133 = %lookup(tp(i1):tp133);
             If i133 <> *zero;
                typeInfo = tpd133(i133);
             Endif;
          Endsl;
          Select;
          When not %found;
             Exsr WriteRecord;
          Other;
             Exsr UpdateRecord;
          Endsl;
       Endfor;

       Endsr;

       //==================================================================
       // Maintenance fields
       //==================================================================
       Begsr  MaintFields;

       QSqte   = qte;
       QSdesc  = desc;
       QSused  = used;
       QSaARH  = aarh;
       QSaARO  = aaro;
       QSaARS  = aars;
       QSrenew = renew;
       //***  Begin Add  ***  06/22/20  ***************************
       QSlocking = locking;
       //***  End   Add  ***  06/22/20  ***************************

       //***  Begin Add  ***  08/31/23  ***************************
       If QSkey  = 'I' or QSkey  = 'i' or QSkey  = 'p' or QSkey  = 'y';
          QSlocking = ' ';
       Endif;
       //***  End   Add  ***  08/31/23  ***************************

       QSuser = q1user;
       QSpgm  = q1pgm;
       QStime = sysTime;
       QSdt   = sysDate;

       Endsr;

       //==================================================================
       // Update records to WTQSP
       //==================================================================
       Begsr  UpdateRecord;

       Exsr MaintFields;

       Update pqs01;

       Endsr;

       //==================================================================
       // Write records to WTQSP
       //==================================================================
       Begsr WriteRecord;

       QSco#  = COco#;
       QSkey  = tp(i1);

       Exsr MaintFields;

       QSstat = 'A';

       QSiUsr = QSuser;
       QSiPgm = QSpgm;
       QSiTim = QStime;
       QSiDt  = QSdt;

       Write pqs0p;

       Endsr;

      /End-free

      //==================================================================
      // Delete records from WTQSP
      //==================================================================
     PDeleteCode       b
     DDeleteCode       pi
     D code                                like(QSkey) const

      /Free
       Dou %eof(wtqsl1);
          Delete (COco#:code) wtqsl1;
       Enddo;
      /End-free

     PDeleteCode       e
**  TYPE TABLE
LQLost                           SUBMISSIONS NOT WRITTEN (LOST)          020120    U
lQRenewal Lost                   SUBMISSIONS NOT WRITTEN (LOST)          021121Y   U
DQDeclined                       SUBMISSIONS NOT WRITTEN (DECLINED)      040140    U
dQRenewal Declined               SUBMISSIONS NOT WRITTEN (DECLINED)      041141Y   U
UQSubmitted                      SUBMISSIONS NOT WRITTEN (SUBMITTED)     060300    U
uQRenewal Submitted              SUBMISSIONS NOT WRITTEN (SUBMITTED)     061301Y   U
VQUnderwriter Review             ACCOUNTS PENDING (UNDERWRITER REVIEW)   080040    U
vQRenewal Underwriter Review     ACCOUNTS PENDING (UNDERWRITER REVIEW)   081041Y   U
IQWaiting                        SUBMISSIONS NOT WRITTEN (WAITING)       100220    L
iQRenewal Waiting                SUBMISSIONS NOT WRITTEN (WAITING)       101221Y   L
AQDuplicate Submission           SUBMISSIONS NOT WRITTEN (DUP SUBMISSION)120240
WWWritten                        SUBMISSIONS WRITTEN                     140080
XWTransfer Out                   ACCOUNTS TRANSFERED (OUT)               160280
TWTransfer In                   WACCOUNTS TRANSFERED (IN)                180260
PQPending                        ACCOUNTS PENDING                        200020    L
pQRenewal Pending                RENEWALS PENDING                        201021Y   L
QQQuoted                         ACCOUNTS QUOTED                         220060    U
qQRenewal Quote                  RENEWALS QUOTED                         221061Y   U
BQBound                          ACCOUNTS BOUND                          240180    U
bQRenewal Bound                  ACCOUNTS BOUND                          241181Y   U
MQBound w/Money                  ACCOUNTS BOUND W/MONEY                  260200
mQRenewal Bound w/Money          ACCOUNTS BOUND W/MONEY                  261201Y
CWCancelled                      ACCOUNTS CANCELLED                      280100
RWReinstated                     ACCOUNTS REINSTATED                     300160
SQClosed                         ACCOUNTS CLOSED                         320320    U
sQRenewal Closed                 ACCOUNTS CLOSED                         321321Y   U
YQAgent Review                   AGENT REVIEW                            090050    L
yQRenewal Agent Review           AGENT REVIEW                            091051Y   L
EQMoved to Casualty              ACCOUNTS MOVED TO CASUALTY              270210 133
eQRenewal Moved to Casualty      ACCOUNTS MOVED TO CASUALTY              271211Y133
EQLC Set Up                      ACCOUNTS PENDING (LC SET UP)            066146 129
eQRenewal LC Set Up              ACCOUNTS PENDING (LC SET UP)            067147Y129
HQLC Complete                    ACCOUNTS PENDING (LC COMPLETE)          068148 129
hQRenewal LC Complete            ACCOUNTS PENDING (LC COMPLETE)          069149Y129
JQApproved - Not Taken           ACCOUNTS LOST (APPROVED - NOT TAKEN)    070150 129
jQRenewal Approved - Not Taken   ACCOUNTS LOST (APPROVED - NOT TAKEN)    071151Y129
KQClosed - Incomplete            ACCOUNTS (CLOSED - INCOMPLETE)          072152 129
kQRenewal Closed - Incomplete    ACCOUNTS (CLOSED - INCOMPLETE)          073153Y129
NQIssue Policy                   ACCOUNTS BOUND (ISSUE POLICY)           074154 129
nQRenewal Issue Policy           ACCOUNTS BOUND (ISSUE POLICY)           075155Y129
**  TYPE TABLE 133
MQMoved to Indemnity             ACCOUNTS MOVED TO INDEMNITY             260200
mQRenewal Moved to Indemnity     ACCOUNTS MOVED TO INDEMNITY             261201Y
**  TYPE TABLE 129
MQApproved                       ACCOUNTS PENDING (APPROVED)             260200
mQRenewal Approved               ACCOUNTS PENDING (APPROVED)             261201Y
