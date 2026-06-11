      * copyright('(c) 2021 Sapiens, Inc. All rights reserved.')
    *===============================================================*
     * SCPYWACTPR Procedure Prototypes for Action pannel of all Policy pgms.
     *========================================================================
    *===============================================================*
    *  Created   : 03/07/12                                         *
    *  Programmer: C:190                                            *
    *  Project   : WC2126                                           *
    *===============================================================*
     * REVISIONS:
      *
      * 04/20/21 C:298 Project: WC2278
      *                Change /include due to nested copy error.
     *
      * 01/07/16 C:65  Project: WC2401
      *                Add prototype for window api.
     *
     * 10/09/15 C:298 Support: 28149
     *                Added Procedure to get Producer Email address
      *                on Policy action Panel
     *
     * 04/29/15 C:65  Project: SY251
     *                Added hidden field for Scriptura Module flag.
     *
     * 02/12/15 C:282 Project: WC2335
     *                Added the handling to hide the policy menu option
     *                from action panel for Endorsement mdoule.
     *
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     * NOTES:
     *
     *========================================================================
      *========================================================================
      * Procedure Prototypes
      *========================================================================

      *------------------------------------------------------------------------
      ***** Beg Chg ***** 04/20/21 ********************************************
     d*****/copy wprPolPop2
     d/include wprPolPop2
      ***** End Chg ***** 04/20/21 ********************************************
     D AgentName       s             40
     D ProdName        s             40
     D ahRRN           s              9  0 inz(0)
     D EM_rrn          s              9  0 inz(0)
     D outRRN          s              9  0 inz(0)
     D saveAhRRN       s              9  0 inz(0)
     D agRRN           s              9  0 inz(0)
     D aaRRN           s              9  0 inz(0)
     D x#RRN           s              9  0 inz(0)
     D xaRRN           s              9  0 inz(0)
     D rpRRN           s              9  0 inz(0)
     D gpRRN           s              9  0 inz(0)
     D xxAgt#          s              5s 0
     D xxNAME#         s              1
     D xxNmType        s              3
     D xxEmpDba        s            100
     D xxEmpFed        s            100
     D cxdate7         s              7  0
     D xxcfdt          s              7  0
     D xxredt          s              7  0
     D date            s              7  0
     D policy#         s             25
     D policy1#        s             25
     D xxRRN1          s              9  0 inz(0)
     D xxBillSts       s             15
     DsysDt7           s              7  0
     DsysDt8           s              8  0
     Dxaco#            s              3  0
     Dxafnd            s              3  0
     Dxaagt#           s              5  0
     Dxaagrc           s              5  0
     Dx2agrc           s              5  0
     Dxaprrc           s              5  0
     Dx2prrc           s              5  0
     Dxagprod          s              5  0
     D fnam            s            100
     D ead1            s             40
     D ead2            s             40
     D ecsz            s             40
     D qty             s             25
     D st              s              2
     D zip             s              9  0
     D fein            s              9  0
     D eadr            s             90
     D flgPEO          s              1
     D stateRptDiv     s              5  0
     D strp            s              1
     D agtMod          s              3  0
     D APrrn           s              9  0 inz(0)
     D FNrrn           s              9  0 inz(0)
     D xxFein          s              9  0
     D incDate7        s              7  0
     D inEmFein        s              9  0
     D x#Fein          s              9  0
     D fCo#            s              3  0
     D fGroup          s              3  0
     D fEmp#           s              9  0
     D fDiv            s              5  0
     D ffyr            s              3  0
     D intRRN1         s              9  0
     D strRRN          s              9
     D empName         s             50
     d ##kc#           s              1
     d ##kl#           s              1
     d ##Qte           s              1
     d ##Mode1         s              1
     d ##Call          s              1
     D ##RRN           s              9
     D #@RRN           s              9  0
     D ##View          s              1
     D actCo#          s              3  0
     D actFnd          s              3  0
     D actEmp#         s              9  0
     D actDiv          s              5  0
     DstrQteLibrary    s             10
     D inEnv           s              1

     d                 ds
     d name                    1    100
     d  dbanm1                 1     40
     d  dbanm2                41    100

     d* Load Action Bar
     d LoadActionBar   pr
     d****** Beg Add ********* 02/12/15 ******************************
     d inPolPln                       1n   options(*nopass)
     d****** End Add ********* 02/12/15 ******************************

     d* State Reporting Division Names
     d STRPName        pr

     d* PEO Names
     d PEOName         pr

     d* Menu Choices
     d Menu            pr

     d* Popup Windows
     d Popup           pr

     DSRADD1           PR                  extpgm('SRADD1')
     D ##co#                          3  0 const
     D ##fnd                          3  0 const
     D ##emp#                         9  0 const
     D ##div                          5  0 const
     D ##fyr                          3  0 const
     D ##name                       100
     D ##fnam                       100
     D ##ead1                        40
     D ##ead2                        40
     D ##ecsz                        40
     D ##qty                         25
     D ##st                           2
     D ##zip                          9  0
     D ##fein                         9  0
     D ##eadr                        90

     DWTQLRRN          pr                  ExtPgm('WTQLRRN')
     D co#                            3  0
     D fnd                            3  0
     D emp#                           9  0
     D div                            5  0
     D env                            1
     D rrn                            9  0

     D*** begin add ***  04/29/15  ************************************************
     d* System Flag API for Scriptura
     d GetScriptura    pr                  extpgm('SA047R')
     d   outSysFlag                   1
     D*** end add *****  04/29/15  ************************************************

     D*****Beg Add ***** 10/09/15 *************************************************
     d* Get Email Address for Producer
     dGetProdInfo      pr                  extpgm('WA002R')
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inAgt#                         5  0 const
     d inANam                        60    const
     d inProd                         5  0 const
     d inPNam                        60    const
     d inArea                         3  0 const
     d inPhon                         7  0 const
     d inPlan                         1    const
     d inDate                         8  0 const
     d inNwRate                       5  2 const
     d inRenRate                      5  2 const
     d inRt#                          3  0 const
     d outProdEmail                  60
     D*****End Add ***** 10/09/15 *************************************************

     d**** begin add ***  01/07/16  ****************************************
     d PopupWindow     pr                  extpgm('WTPOPUP')
     d outProgram_                   40    const
     d outRRN_                        9  0 const
     d outMode_                       2    const
     d outData_                     500    const
     d**** end add *****  01/07/16  ****************************************
