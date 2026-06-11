     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H nomain
     *===============================================================================
     *
     * SPIENV - Procedures for determining environment/system/machine
     *
     * Created   : 05/26/15
     * Programmer: C:65
     * Project   : SY251
     *
     *===============================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 08/15/22 c:65 Project WC2670                                                               *
      *               Change default environment to 'Q' when no production libary found.           *
      *                                                                                            *
      * 03/04/21 c:62 Project WC2761                                                               *
      *               Add data center check                                                        *
      *                                                                                            *
     *===============================================================================

      * File

     d/copy sprenv

     d psds          esds                  extname(szq1p)

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Get client ID
     d CheckObj        pr                  extpgm('SRCHKOBJ')
     d   outObjName                  10
     d   outObjType                   7
     d   outObjErr                    1

     d* Check Data Center
     d ChkDtaCtr       pr                  extpgm('CHKDTACTR')
     d   isDataCtr                     n

      *=====================================================================
      * CheckEnv - Check the environment of system.
      *=====================================================================
     p CheckEnv        b                   EXPORT
     d CheckEnv        PI             1A

     D clientID        s              3    inz('000')
     D instanceName    s             10    inz(' ')
     D objType         s              7    inz(' ')
     D error           s              1    inz(' ')
      /free

       GetClientID(clientID);

       objType = '*LIB';
       error = ' ';
       instanceName = 'PF' + clientID + 'PN';

       //* Check for production machine instance library.
       CheckObj(instanceName:objType:error);

       If error = '0';
          Return 'P';
       Endif;

       error = ' ';
       instanceName = 'PF' + clientID + 'QA';

       //* Check for QA machine instance library.
       CheckObj(instanceName:objType:error);

       If error = '0';
          Return 'Q';
       Endif;

       //*** begin chg ***  08/15/22  **********************************
       //*****Return ' ';
       Return 'Q';
       //*** end chg *****  08/15/22  **********************************

      /end-free

     p CheckEnv        e

      *=====================================================================
      * CheckDataCenter - Check to see if at Data Center(ASDP07).
      *=====================================================================
     p CheckDataCenter...
     p                 b                   EXPORT
     d CheckDataCenter...
     d                 PI              n

     D isDataCtr       s               n   inz(*Off)
      /free

       ChkDtaCtr(isDataCtr);

       Return isDataCtr;

      /end-free

     p CheckDataCenter...
     p                 e
