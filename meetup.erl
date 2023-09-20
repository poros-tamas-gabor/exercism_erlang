-module(meetup).

-export([meetup/4]).


dayofweek_to_integer(monday) -> 1; 
dayofweek_to_integer(tuesday) -> 2; 
dayofweek_to_integer(wednesday) -> 3; 
dayofweek_to_integer(thursday) -> 4; 
dayofweek_to_integer(friday) -> 5; 
dayofweek_to_integer(saturday) -> 6; 
dayofweek_to_integer(sunday) -> 7. 

find_first_dayofweek(Year, Month, Day, DayOfWeek) ->
    case calendar:day_of_the_week(Year, Month, Day) =:= dayofweek_to_integer(DayOfWeek) of
        true -> {Year, Month, Day};
        false -> find_first_dayofweek(Year, Month, Day + 1, DayOfWeek)
    end.

find_last_dayofweek(Year, Month, Day, DayOfWeek) ->
    case calendar:valid_date(Year, Month, Day) of
        true -> 
            case calendar:day_of_the_week(Year, Month, Day) =:= dayofweek_to_integer(DayOfWeek) of
                true -> {Year, Month, Day};
                false -> find_last_dayofweek(Year, Month, Day - 1, DayOfWeek) end;
        false -> find_last_dayofweek(Year, Month, Day - 1, DayOfWeek)
    end.


meetup(Year, Month, DayOfWeek, first) -> find_first_dayofweek(Year, Month, 1, DayOfWeek);
meetup(Year, Month, DayOfWeek, second) -> 
    {Y, M, D} = find_first_dayofweek(Year, Month, 1, DayOfWeek),
    {Y, M, D + 7};
meetup(Year, Month, DayOfWeek, third) -> 
    {Y, M, D} = find_first_dayofweek(Year, Month, 1, DayOfWeek),
    {Y, M, D + 2*7};
meetup(Year, Month, DayOfWeek, fourth) -> 
    {Y, M, D} = find_first_dayofweek(Year, Month, 1, DayOfWeek),
    {Y, M, D + 3*7};
meetup(Year, Month, DayOfWeek, fifth) -> 
    {Y, M, D} = find_first_dayofweek(Year, Month, 1, DayOfWeek),
    {Y, M, D + 4*7};

meetup(Year, Month, DayOfWeek, last) -> 
    find_last_dayofweek(Year, Month, 31, DayOfWeek);

meetup(Year, Month, DayOfWeek, teenth) -> find_first_dayofweek(Year, Month, 13, DayOfWeek).