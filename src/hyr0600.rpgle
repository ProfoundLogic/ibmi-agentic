**free

dcl-f hyrstubd workstn;

pgmname = 'HYR0600';
title   = 'Shipment Processing';
line1   = 'Main interactive screen for picking, packing, and';
line2   = 'completing shipments.  Drives HYD0600A/B subfiles';
line3   = 'over HYPSGCU (shipment group/customer) and chains';
line4   = 'to HYR0602 (detail) and HYR0608 (tote processing).';

exfmt stub;

*inlr = *on;
