update Documents set publisher=institution where type = "ConferenceProceedings" and institution != "";
update Documents set institution="" where type = "ConferenceProceedings" and institution != "";
--SELECT type,title,institution,publisher FROM Documents where type = "ConferenceProceedings";
