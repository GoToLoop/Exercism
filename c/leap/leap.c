#include "leap.h"
short leap_year(short year) { return !( year % 100 ? year % 4 : year % 400 ); }
