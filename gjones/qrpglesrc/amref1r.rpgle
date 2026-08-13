      // @MBRTEXT: Demo of RPG Lite for AMREF
         ctl-opt dftactgrp(*no);

         dcl-f amref1d workstn(*ext) handler('PROFOUNDUI(HANDLER)');

         dou btnexit = '1';
            exfmt main;
         enddo;

         return;