     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('WTBNDDIR')
     H*====================================================================
     H/TITLE FIXALIAS - Fix Alias's
     H*====================================================================
     H*  Description:
     H*
     H*  This program will make sure alias's point to the correct library
     H*
     H*
     H*====================================================================
     H*  Created on 09/21/2023 by c:62 for Ticket 113606(099)
     H*====================================================================
     H*  Revisions:
     H*
     H*====================================================================
     d strLivLib       s             10
     d strQteLib       s             10
     d strSql          s            500

      /Copy wPrEmplr

        strLivLib = %Trim(RtvQuoteLib('*LIV'));
        strQteLib = %Trim(RtvQuoteLib('*QTE'));

        // Drop all alias's
        // Live
        Exec SQL Drop Alias WDEAP_hld;
        Exec SQL Drop Alias WASRP_hld;
        Exec SQL Drop Alias WHFRP_hld;
        Exec SQL Drop Alias WHFRP_hst;

        // Quote
        strSql = 'Drop Alias ' + %Trim(strQteLib) + '/WDEAP_hld';
        Exec SQL Execute Immediate :strSql;

        // Create alias's
        // WDEAP - Hold
        //Live
        Exec SQL CREATE ALIAS wdeap_hld
                   FOR WDEAP(HOLD);
        Exec SQL LABEL ON ALIAS WDEAP_HLD
                   IS 'WDEAP(HOLD) - Holding Area';

        // Quote
        strSql = 'Create Alias ' + %Trim(strQteLib) + '/WDEAP_hld ' +
        'for ' + %Trim(strQteLib) + '/wdeap(hold)';
        Exec SQL Execute Immediate :strSql;
        strSql = 'Label on Alias ' + %Trim(strQteLib) + '/WDEAP_hld ' +
        'is ''WDEAP(HOLD) - Holding Area''';
        Exec SQL Execute Immediate :strSql;


        // WASRP - Hold
        Exec SQL CREATE ALIAS wasrp_hld
                   FOR WASRP(HOLD);
        Exec SQL LABEL ON ALIAS WASRP_HLD
                   IS 'WASRP(HOLD) - Holding Area';

        // WHFRP - Hold
        Exec SQL CREATE ALIAS whfrp_hld
                   FOR WHFRP(HOLD);
        Exec SQL LABEL ON ALIAS WHFRP_HLD
                   IS 'WHFRP(HOLD) - Holding Area';

        // WHFRP - History
        Exec SQL CREATE ALIAS whfrp_hst
                   FOR WHFRP(HISTORY);
        Exec SQL LABEL ON ALIAS WHFRP_HST
                   IS 'WHFRP(HISTORY) - FROI History';

        *inLR = *On;
