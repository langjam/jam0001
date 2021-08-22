import { parse } from "./parse-that/parser0001";
import { words as dictionary } from "./purr-on/words";

const usage_str = "Usage: node purr 'your purr program here'";

const purr = () => {
    // console.log("Purr\n");
    // console.log(process.argv);
    if (!process.argv[2]) {
        console.error("no input passed to purr\n", usage_str)
        return 1;
    }
    const purr_input = process.argv[2].replace(/\\n/g, '\n '); 
    const parsed_purr = parse(purr_input);
    // while (parsed_purr) {
    //     const word = parsed_purr.shift();
    //     // if (word && dictionary[word]) {

    //     // }
    // }
}

purr();