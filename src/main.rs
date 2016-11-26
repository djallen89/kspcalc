extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::str::FromStr;
use std::collections::HashMap;
use parse::{Token, TokenStream};

pub mod parse;

static NUM_VAR: &'static str = "Only enter a variable or a number.";

pub type Variables = HashMap<String, f64>;

#[derive(Clone)]
pub enum ArgType {
    Decimal(f64),
    Symbol(String)
}

impl ArgType {
    pub fn literalize(self, vars: &Variables) -> Option<f64> {
        match self {
            ArgType::Decimal(x) => Some(x),
            ArgType::Symbol(ref s) => {
                if let Some(x) = vars.get(s) {
                    Some(*x)
                } else {
                    None
                }
            }
        }
    }
}

pub type ArgList = Vec<ArgType>;

#[derive(Clone, Copy, Debug)]
pub enum RocketScience {
    TWR,
    Define,
    Display
}

impl FromStr for RocketScience {
    type Err = String;
    fn from_str(word: &str) -> Result<RocketScience, String> {
        match word {
            "twr" => Ok(RocketScience::TWR),
            "display" => Ok(RocketScience::Display),
            "define" => Ok(RocketScience::Define),
            x => Err(x.to_string())
        }
    }
}

impl RocketScience {
    pub fn arity(&self) -> usize {
        match self {
            &RocketScience::TWR => 3,
            &RocketScience::Define => 2,
            &RocketScience::Display => 1,
        }
    }

    pub fn helper(self, vars: &mut Variables, ed: &mut Editor<()>) {
        let result = match self {
            RocketScience::TWR => Some(twr_argumenter(vars, ed)),
            RocketScience::Display => {
                display_helper(ed, vars);
                None
            },
            RocketScience::Define => {
                let msg = define_argumenter(vars, ed);
                println!("{}", msg);
                None
            }
        };
        update_latest(result, vars);
    }

    pub fn exec(self, args: Vec<ArgType>, vars: &mut Variables) {
        let result = match self {
            RocketScience::TWR => Some(twr_arged(args, vars)),
            RocketScience::Display => {
                display_arged(args, vars);
                None
            },
            RocketScience::Define => {
                let msg = define_arged(args, vars);
                println!("{}", msg);
                None
            }
        };
        update_latest(result, vars);
    }
}

pub fn update_latest(result: Option<Result<f64, String>>, vars: &mut Variables) {
    if let Some(r) = result {
        match r {
            Ok(x) => if let Some(_) =  vars.insert("latest".to_string(), x) {},
            Err(f) => println!("{}", f)
        }
    }
}
        
fn get_val(prompt: &str, ed: &mut Editor<()>, err: &str) -> Option<Token> {
    loop {
        let readline = ed.readline(prompt);
        match readline {
            Ok(line) => {
                ed.add_history_entry(&line);
                let mut tokenstream =
                    TokenStream::new(line.to_string(),
                                     vec!(parse::is_op, parse::is_var, parse::is_number),
                                     err.to_string());
                match tokenstream.next() {
                    Some(Ok(x)) => return Some(x),
                    Some(Err(f)) => {
                        println!("{}", f);
                        continue
                    },
                    None => continue
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                continue
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                return None
            },
            Err(err) => {
                println!("Error: {:?}", err);
                return None
            }
        }
    }
}
        
fn get_float(prompt: &str, ed: &mut Editor<()>, vars: &Variables) -> Option<f64> {
    loop { 
        if let Some(t) = get_val(prompt, ed, NUM_VAR) {
            if let Ok(r) = make_arg(t) {
                return r.literalize(vars)
            }
        } else {
            continue
        }
    }
}

fn get_integer(prompt: &str, ed: &mut Editor<()>, vars: &Variables) -> Option<usize> {
    loop {
        if let Some(i) = get_float(prompt, ed, vars) {
            if i == i.round() {
                return Some(i as usize)
            } else {
                println!("Please enter an integer.");
                continue
            }
        }
    }
}

fn get_string(prompt: &str, ed: &mut Editor<()>) -> Option<String> {
    loop { 
        if let Some(Token::Variable(s)) =
            get_val(prompt, ed, "Enter a string for the name of the variable.") {
            return Some(s)
        } else {
            continue
        }
    }
}

fn polar_question(ed: &mut Editor<()>) -> Option<bool> {
    loop {
        match get_val("y/n", ed, "Expecting y, yes, n or no.") {
            Some(Token::Variable(ref msg)) => match msg.to_lowercase().as_ref() {
                "y" | "yes" => return Some(true),
                "n" | "N" => return Some(false),
                x => {
                    println!("{} is not an acceptable response.", x);
                    continue
                }
            },
            Some(Token::Literal(_)) => {
                println!("Numbers do not answer yes or no questions.");
                continue
            },
/*            Some(Token::Colon) => {
                println!("Careful, we're not using ternary operators.");
                continue
            },*/
            Some(Token::Operator(_)) => {
                println!("Yes, y, no or n");
                continue
            }
            None => return None
        }
    }
}

fn parse(command: String, vars: &mut Variables, rl: &mut Editor<()>) {
    let mut args = TokenStream::new(command,
                                    vec!(parse::is_op, /*parse::is_colon,*/ parse::is_var, parse::is_number),
                                    "Not a valid token.".to_string());
    match args.next() {
        None => {},
        Some(Ok(Token::Operator(rs))) => argumenter(rs, &mut args, vars, rl),
        Some(Ok(x)) => println!("Exptected function, found {:?}", x),
        Some(Err(f)) => println!("{}", f)
    }
}

fn make_arg(token: Token) -> Result<ArgType, String> {
    match token {
        Token::Literal(x) => Ok(ArgType::Decimal(x)),
        Token::Variable(s) => Ok(ArgType::Symbol(s)),
        x =>  Err(format!("Unexpected token `{:?}`", x))
    }
}    

fn arg_gatherer(tokenstream: &mut TokenStream<Token, String>) -> Result<Vec<ArgType>, String> {
    let mut args = Vec::new();
    for token in tokenstream {
        args.push(try!(make_arg(try!(token))));
    }
    Ok(args)
}

fn argumenter(rs: RocketScience, tokens: &mut TokenStream<Token, String>, vars: &mut Variables, ed: &mut Editor<()>) {
    let args = match arg_gatherer(tokens) {
        Ok(x) => x,
        Err(f) => {
            println!("{}", f);
            return
        }
    };
    match (args.len() == 0, args.len() == rs.arity()) {
        (true,      _) => rs.helper(vars, ed),
        (false,  true) => rs.exec(args, vars),
        (false, false) => {
            println!("Sorry, arity mismatch. Please try again using {} arguments, or none for a guided function",
                     rs.arity());
        }
    }
}

fn define(name: String, val: f64, vars: &mut Variables) -> String {
    if let Some(p) = vars.insert(name.clone(), val) {
        format!("Updated {} from {} to {}.", name, p, val)
    } else {
        format!("Defined {} as {}.", name, val)
    }
}

fn define_arged(args: Vec<ArgType>, vars: &mut Variables) -> String {
    match (args[0].clone(), args[1].clone().literalize(vars)) {
        (ArgType::Symbol(s), Some(x)) => define(s, x, vars),
        (ArgType::Symbol(_),    None) => format!("Cannot bind unknown value."),
        (                 _,       _) => format!("Only strings beginning with a letter can be symbols.")
    }
}

fn define_argumenter(vars: &mut Variables, ed: &mut Editor<()>) -> String {
    println!("Enter the name of the variable to be (re)defined.");
    let name = if let Some(s) = get_string("name: ", ed) {
        s
    } else {
        return "Nothing changed.".to_string()
    };
    
    println!("Do you know the value you want to assign to this name?");
    if let Some(b) = polar_question(ed) {
        if b {
            if let Some(v) = get_float("value: ", ed, vars) {
                define(name, v, vars)
            } else {
                format!("Could not define {}", name)
            }
        } else {
            println!("Please input the function you wish to compute.");
            readline_handler(">> ", ed, vars);
            match vars.clone().get("latest") {
                Some(x) => define(name, *x, vars),
                _ => format!("Could not define {}", name)
            }
        }
    } else {
        format!("Cannot bind an unknown value to a name.")
    }
}
    
fn display_arged(args: Vec<ArgType>, vars: &mut Variables) {
    if let Some(x) = args[0].clone().literalize(vars) {
        println!("{}", x);
    }
}

fn display_helper(ed: &mut Editor<()>, vars: &mut Variables) {
    println!("What would you like to display?");
    if let Some(x) = get_float(">> ", ed, vars) {
        println!("{}", x);
    }
}

fn sum_thrust(vars: &Variables, ed: &mut Editor<()>) -> Result<f64, String> {
    let mut total_thrust = 0.0;
    println!("Please enter the number of different kinds of engines there are on this stage.");
    let engine_varieties = match get_integer("varieties: ", ed, vars) {
        Some(i) => i,
        None => return Err("variety".to_string())
    };

    for variety in 0 .. engine_varieties {
        println!("Please enter the number of engines of type {}", variety + 1);
        let number_of_engines = match get_integer("quantity: ", ed, vars) {
            Some(n) => n,
            None => return Err("number.".to_string())
        };
        println!("Please enter the thrust of engine type {}", variety + 1);
        let engine_thrust = match get_float("thrust: ", ed, vars) {
            Some(x) => x,
            None => return Err("thrust".to_string())
        };

        total_thrust += number_of_engines as f64 * engine_thrust;
    }

    Ok(total_thrust)
}

fn twr_argumenter(vars: &Variables, ed: &mut Editor<()>) -> Result<f64, String> {
    let twr_err = "Can't calculate TWR without knowing ";
    println!("Do you know the total thrust?");
    let thrust;
    if let Some(b) = polar_question(ed) {
        if b {
            match get_float("thrust: ", ed, vars) {
                Some(x) => {
                    thrust = x;
                },
                None => return Err(format!("{} thrust.", twr_err))
            }
        } else { 
            match sum_thrust(vars, ed) {
                Ok(x) => {
                    thrust = x;
                },
                Err(f) => return Err(format!("{} about {} of engines.", twr_err, f))
            }
        }
    } else {
        return Err(format!("{} thrust.", twr_err))
    }
    
    println!("What is the mass of the rocket?");
    let mass = match get_float("mass: ", ed, vars) {
        Some(x) => x,
        None => return Err(format!("{} mass", twr_err))
    };

    let g = match get_float("gravitational constant: ", ed, vars) {
        Some(x) => x,
        None => return Err(format!("{} gravitational constant", twr_err))
    };
    
    Ok(twr(thrust, mass, g))
}    

fn twr_arged(args: Vec<ArgType>, vars: &Variables) -> Result<f64, String> {
    let err_prefix = "Cannot calculate twr without knowing ";
    let g_postfix = "the gravitational constant.";
    let m_postfix = "the mass of the rocket.";
    let f_postfix = "the thrust of the rocket.";
    match (args[0].clone().literalize(vars), args[1].clone().literalize(vars), args[2].clone().literalize(vars)) {
        (Some(f), Some(m), Some(g)) => Ok(twr(f, m, g)),
        (Some(_), Some(_),    None) => Err(format!("{} {}", err_prefix, g_postfix)),
        (Some(_),    None, Some(_)) => Err(format!("{} {}", err_prefix, m_postfix)),
        (   None, Some(_), Some(_)) => Err(format!("{} {}", err_prefix, f_postfix)),
        (   None, Some(_),    None) => Err(format!("{} {} and {}", err_prefix, f_postfix, g_postfix)),
        (   None,    None, Some(_)) => Err(format!("{} {} and {}", err_prefix, f_postfix, m_postfix)),
        (Some(_),    None,    None) => Err(format!("{} {} and {}", err_prefix, g_postfix, m_postfix)),
        (   None,    None,    None) => Err(format!("{} {}, {}, and {}", err_prefix, f_postfix, m_postfix, g_postfix))
    }    
}
                   
fn twr(thrust: f64, mass: f64, g: f64) -> f64 {
    thrust / (mass * g)
}

fn readline_handler(prompt: &str, ed: &mut Editor<()>, vars: &mut Variables) -> Option<()> {
    let readline = ed.readline(prompt);
    match readline {
        Ok(line) => {
            ed.add_history_entry(&line);
            match line.to_lowercase().trim().as_ref() {
                "exit" | "quit" | ",q" => None,
                _ => {
                    parse(line, vars, ed);
                    Some(())
                }
            }
        },
        Err(ReadlineError::Interrupted) => {
            println!("CTRL-C");
            None
        },
        Err(ReadlineError::Eof) => {
            println!("CTRL-D");
            None
        },
        Err(err) => {
            println!("Error: {:?}", err);
            None
        }
    }
}    

fn main() {
    let mut vars = Variables::new();
    let mut rl = Editor::<()>::new();
    if let Err(_) = rl.load_history("history.txt") {
        println!("No previous history.");
    }
    loop {
        if let None = readline_handler(">> ", &mut rl, &mut vars) {
            break;
        }
    }
    match rl.save_history("history.txt") {
        Ok(_) => {},
        Err(f) => {
            println!("{}", f);
        }
    }
}

