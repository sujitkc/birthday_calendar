exception Birthday_Calendar_exception of string

let bday_messages = [
  "Happy birthday";
  "Wish you a very happy birthday";
  "Wish you a very happy birthday and an awesome year ahead";
  "Have a great birthday and a fabulous year";
  "Have a happy birthday and a great year ahead";
  "Many happy returns of the day";
  "Many many happy returns of the day"
]

let month_num  = function
  | Calendar.January   -> 1
  | Calendar.February  -> 2
  | Calendar.March     -> 3
  | Calendar.April     -> 4
  | Calendar.May       -> 5
  | Calendar.June      -> 6
  | Calendar.July      -> 7
  | Calendar.August    -> 8
  | Calendar.September -> 9
  | Calendar.October   -> 10
  | Calendar.November  -> 11
  | Calendar.December  -> 12

let month_of_int n =
  match n with
  |  1 -> Calendar.January
  |  2 -> Calendar.February
  |  3 -> Calendar.March
  |  4 -> Calendar.April
  |  5 -> Calendar.May
  |  6 -> Calendar.June
  |  7 -> Calendar.July
  |  8 -> Calendar.August
  |  9 -> Calendar.September
  | 10 -> Calendar.October
  | 11 -> Calendar.November
  | 12 -> Calendar.December
  |  _ -> failwith ("Invalid month number " ^ (string_of_int n))

module Record = struct
  type record = {
    roll_num : string;
    name     : string;
    birthday : Calendar.date;
    gender   : string;
  }

  let compare_birthdays bday1 bday2 =
    let Calendar.Date(d1, m1, _) = bday1
    and Calendar.Date(d2, m2, _) = bday2 in
   let n1 = month_num m1
    and n2 = month_num m2 in
    if n1 < n2 then -1
    else if n1 > n2 then 1
    else
      if d1 < d2 then -1
      else if d1 > d2 then 1
      else 0

  let sort_records records =
    List.sort (
      fun r1 r2 ->
        let bday1 = r1.birthday
        and bday2 = r2.birthday in
        compare_birthdays bday1 bday2
    ) records

  let string_of_record r =
    "{ " ^ r.roll_num ^ "; " ^ r.name ^ "; " ^ (Calendar.string_of_date r.birthday) ^ " }"
end

let csv_to_record_list file_name =
  let allrows = Csv.load file_name in
  match allrows with
    [] ->  raise (Birthday_Calendar_exception "empty input")
  | _  ->
    List.map
    (
      fun row ->
        match row with
          [ rn; name;  strdate; g ] ->
            let dlist = Str.split (Str.regexp "/") strdate in
            begin
              match dlist with
                [ strd; strm; stry ] ->
                  {
                    Record.roll_num = rn;
                    Record.name = name;
                    Record.birthday = Calendar.Date(
                      (int_of_string strd),
                      (month_of_int (int_of_string strm)),
                      (int_of_string stry)
                    );
                    Record.gender = g;
                  }
              | _ -> failwith ("Couldn't understand date " ^ strdate)
            end
        | _ -> raise (Birthday_Calendar_exception "Invalid CSV row")
    )
    allrows

let pick_from_list l =
  let index = Random.int (List.length l) in
  List.nth l index

let birthday_frame flowers r =
  let Calendar.Date(d, m, _) = r.Record.birthday in
  let bday = (Calendar.string_of_month m) ^ " " ^ (string_of_int d) in
  let colour = if r.Record.gender = "M" then "blue" else "red" in
  let slide_header = "\\begin{frame}{\\color{" ^ colour ^ "}Happy Birthday " ^ "}\n{" ^
    bday ^ "}\n\\begin{center}\n"
  and msg = (pick_from_list bday_messages) ^ "! \\\\ \\vspace{0.5cm}{\Large " ^
    r.Record.name ^ "} (" ^ r.Record.roll_num ^ ")"
  and flower = (pick_from_list flowers) in
  let flower_image = "\\includegraphics[height=0.5\\textheight]{flowers/" ^ flower ^ "}\n\n"
  and slide_footer = "\n\\end{center}\n\\end{frame}\n"
  in
  slide_header ^ flower_image ^ msg ^ slide_footer

(* Reads an input file line by line and returns a list of lines. *)
let read_lines_from_file filename = 
  let chan = open_in filename in
  let rec iter lines =
    try
      let line = input_line chan in (iter (line::lines))
    with End_of_file ->
      close_in chan;
      List.rev lines
  in
  iter []


(* Reads an entired file into a single string. *) 
let read_file filename = 
  let lines = read_lines_from_file filename in
  (List.fold_left (fun a b -> (a ^ "\n" ^ b)) "" lines)

(* Generate the latex file for the attendance list. *)
let gen_latex_file latex =
  let ochan = open_out ("output/" ^ "birthday_calendar.tex") in
    output_string ochan latex;
    close_out ochan

let gen_birthday_calendar () =
  let num_of_args = Array.length Sys.argv in
  if num_of_args <> 2 then
    print_string "Exactly one command-line argument required.\n"
  else
  let flowers = Array.to_list (Sys.readdir "output/flowers")
  and records = csv_to_record_list Sys.argv.(1) in
  let sorted_records = (Record.sort_records records) in
  let frames = List.map (birthday_frame flowers) sorted_records in
  let all_wishes = List.fold_left (fun x y -> x ^ y) "" frames in
  let before = read_file "before.tex" in
  let after = "\n" ^ "\\end{document}" in
  let latex = before ^ all_wishes ^ after in
  gen_latex_file latex

let main () =
  gen_birthday_calendar ()

let _ = main ()
