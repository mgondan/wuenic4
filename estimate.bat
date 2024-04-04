rem estimate.bat afg
swipl -g "consult('swipl/wuenic_ver_4.pl'), consult('countries/%1.pl'), estimate" -g halt 2>warn40.txt
