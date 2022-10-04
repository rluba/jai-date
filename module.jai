Polymorphic_Date :: struct (T: Type) {
    year: s16;
    month: T;
    day: T;
}

Date :: Polymorphic_Date(s8);

Date_Format :: enum {
    ISO;
    DE;
}

Date_Part :: enum {
	YEAR;
	MONTH;
	DAY;
	YEARS :: YEAR;
	MONTHS :: MONTH;
	DAYS :: DAY;
}

add :: (date: *Date, amount: int, unit := Date_Part.DAY) {
    big_date: Polymorphic_Date(int);
    big_date.year = date.year;
    big_date.month = date.month;
    big_date.day = date.day;

    if #complete unit == {
        case .DAY;      big_date.day   += amount;
        case .MONTH;    big_date.month += amount;
        case .YEAR;     big_date.year  += cast(s16) amount;
    }

    normalize_date(*big_date);

    date.year = big_date.year;
    date.month = cast(s8) big_date.month;
    date.day = cast(s8) big_date.day;
}


get_current_date :: () -> Date {
	cal := to_calendar(current_time_consensus());
	date: Date;
	date.year = cast(s16) cal.year;
	date.month = cal.month_starting_at_0 + 1;
	date.day = cal.day_of_month_starting_at_0 + 1;
	return date;
}

to_apollo_time :: (date: Date) -> Apollo_Time {
	cal: Calendar_Time;
	cal.year = date.year;
	cal.month_starting_at_0 = date.month - 1;
	cal.day_of_month_starting_at_0 = date.day - 1;
	return calendar_to_apollo(cal);
}

date_from_unix_timestamp :: (timestamp: s64, time_zone := Time_Zone.UTC) -> Date {
	timestamp_copy := timestamp; // Because we have to pass a pointer below…
	tmp: tm;
	if time_zone == .LOCAL {
		tmp = localtime_r(*timestamp_copy, *tmp);
	} else {
		tmp = gmtime_r(*timestamp_copy, *tmp);
	}

	date: Date;
	date.year = cast(s16) tmp.tm_year + 1900;
	date.month = cast(s8) tmp.tm_mon + 1;
	date.day = cast(s8) tmp.tm_mday;

	assert(is_normalized(date));

	return date;
}

parse_date :: (str: string, format := Date_Format.ISO) -> Date, bool {
    result: Date;
    if #complete format == {
        case .ISO;
            if !iso_date_expr.prog {
                iso_date_expr = compile("^(\\d{4})-(\\d{2})-(\\d{2})");
            }
            matched, captures := match(str, iso_date_expr);
            if !matched        return .{}, false;
            defer array_free(captures);

            result.year = cast(s16) to_integer(captures[1]);
            result.month = cast(s8) to_integer(captures[2]);
            result.day = cast(s8) to_integer(captures[3]);
        case .DE;
            if !de_date_expr.prog {
                de_date_expr = compile("^(\\d{2}).(\\d{2}).(\\d{2,4})");
            }
            matched, captures := match(str, de_date_expr);
            if !matched        return .{}, false;
            defer array_free(captures);

            result.year = cast(s16) to_integer(captures[3]);
            result.month = cast(s8) to_integer(captures[2]);
            result.day = cast(s8) to_integer(captures[1]);
            if result.year < 100 {
                result.year += 2000;
            }
    }

    return result, is_normalized(result);
}

string_from_date :: (date: Date, format := Date_Format.ISO, allocator := context.allocator) -> string {
    builder: String_Builder;
	builder.allocator = allocator;
    defer free_buffers(*builder);
    append_date(*builder, date, format);
    return builder_to_string(*builder, allocator);
}

append_date :: (builder: *String_Builder, date: Date, format := Date_Format.ISO) {
    if #complete format == {
        case .ISO;
            print_to_builder(builder, "%-%-%", date.year, formatInt2(date.month), formatInt2(date.day));
        case .DE;
            print_to_builder(builder, "%3.%2.%1", date.year, formatInt2(date.month), formatInt2(date.day));
    }
}

// Stupidly simple. Could be done better
normalize_date :: (date: *$T/Polymorphic_Date) {
    if date.day < 1 {
        while date.day < 1 {
            date.month -= 1;
            normalize_month(date);
            days := get_days_in_month(<<date);
            date.day += days;
        }
    } else {
        normalize_month(date);
        days := get_days_in_month(<<date);
        while date.day > days {
            date.day -= days;
            date.month += 1;
            normalize_month(date);
            days = get_days_in_month(<<date);
        }
    }
}

is_normalized :: (date: $T/Polymorphic_Date) -> bool {
    return date.month >= 1 && date.month <= 12 && date.day >= 1 && date.day <= get_days_in_month(date);
}
#assert(is_normalized(Date.{2000,2,29}));
#assert(is_normalized(Date.{2000,12,31}));
#assert(is_normalized(Date.{0,12,31}));
#assert(!is_normalized(Date.{2000,13,31}));
#assert(!is_normalized(Date.{2000,12,32}));
#assert(!is_normalized(Date.{2000,12,0}));
#assert(!is_normalized(Date.{2000,0,1}));


is_leap_year :: (year: int) -> bool {
    if year % 4        return false;

    if (year % 100) == 0 && (year % 400)    return false;

    return true;
}

#assert(is_leap_year(2000));
#assert(!is_leap_year(2003));
#assert(is_leap_year(2004));
#assert(!is_leap_year(1999));
#assert(!is_leap_year(1900));
#assert(!is_leap_year(2100));

compare :: (a: Date, b: Date) -> int {
	if a.year != b.year		then return a.year - b.year;
	if a.month != b.month	then return a.month - b.month;
	return a.day - b.day;
}

#scope_file

normalize_month :: (date: *$T/Polymorphic_Date) {
    while date.month < 1 {
        date.month += 12;
        date.year -= 1;
    }
    while date.month > 12 {
        date.month -= 12;
        date.year += 1;
    }
}

// Date must already be normalized!
get_days_in_month :: (date: $T/Polymorphic_Date) -> int {
    if date.month == {
        case 2;
            if is_leap_year(date.year)    return 29;
            return 28;
        case 4; #through;
        case 6; #through;
        case 9; #through;
        case 11;
            return 30;
        case;
            return 31;
    }
}

iso_date_expr: Regexp;
de_date_expr: Regexp;

formatInt2 :: #bake_arguments formatInt(minimum_digits = 2);

#import "Basic";
#import "uniform";
#if OS != .WINDOWS {
	#import "POSIX";
}

