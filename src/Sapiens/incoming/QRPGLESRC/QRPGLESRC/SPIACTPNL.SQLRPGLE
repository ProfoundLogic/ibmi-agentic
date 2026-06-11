     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H nomain
     H*================================================================
     H* Revisions:
     H*================================================================
     d* Copybooks
     d/copy sPrModule
     p*====================================================================
     p* Get Action Panel Menu Items to be display
     P*====================================================================
     PGetActPnlItems   b                   Export
     dGetActPnlItems   pi

     d outMenuChoices               500a   Varying
     d outMenuValues                100a   Varying

       outMenuChoices = 'Application Configuration';
       outMenuValues  = 'WTAPPCFG';

       // Claims
       If isModEnabled('WorkComp');
          outMenuChoices = %Trim(outMenuChoices) + ', Claims Configuration';
          outMenuValues = outMenuValues + ',WTCLMCFG';
       Endif;

       // Managed Care
       If isModEnabled('Managed Care');
          outMenuChoices = outMenuChoices + ', Managed Care Configuration';
          outMenuValues = outMenuValues + ',MTINTCFG';
       Endif;

       // Policy is always loaded
       outMenuChoices = outMenuChoices + ', Policy Configuration';
       outMenuValues = outMenuValues + ',WTPOLCFG';

       // Portal
       If isModEnabled('Portal Module');
          outMenuChoices = outMenuChoices + ',' +
          GetModuleProperty('Portal Module':'Name') + ' Configuration';
          outMenuValues = %trim(outMenuValues) + ',WICONFIG';
       Endif;
     PGetActPnlItems   e

