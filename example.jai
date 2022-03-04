main :: () {
	date, success := parse_date("2020-02-15");
	if !success exit(1);

	add(*date, 15, .DAYS);
	add(*date, 3, .MONTHS);

	log("Date is now %", string_from_date(date, allocator = __temporary_allocator));
}

#import "Basic";

#load "module.jai";
