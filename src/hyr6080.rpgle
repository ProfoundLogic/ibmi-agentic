**free

dcl-f hyrstubd workstn;

pgmname = 'HYR6080';
title   = 'Order Status Email (batch)';
line1   = 'Sends shipment-notification email to a customer and';
line2   = 'logs it to HYPEELD / HYPELG.  Normally invoked by';
line3   = 'HYR0614 (the shipping orchestrator) after a shipment';
line4   = 'is completed and the tracking number is known.';

exfmt stub;

*inlr = *on;
