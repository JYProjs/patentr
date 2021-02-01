#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string>

using namespace Rcpp;

// checks if string starts w/ a prefix
// WORKS (basic test cases)
bool startsWith(std::string text, std::string prefix)
{
    return (text.find(prefix) == 0);
}

// remove whitespace from beginning of a string
// WORKS (basic test cases)
void stripBeginWhitespace(std::string &text)
{
    int start = 0;
    while (text[start] == ' ' || text[start] == '\n' || text[start] == '\t' || text[start] == '\r')
        start++;
    text = text.substr(start);
}

// remove whitespace from end of a string
// WORKS (basic test cases)
void stripEndWhitespace(std::string &text)
{
    int end = text.length();
    while (text[end - 1] == ' ' || text[end - 1] == '\n' || text[end - 1] == '\t' || text[end - 1] == '\r')
      end--;
    text = text.substr(0, end);
}

// [[Rcpp::export]]
int txt_to_df_cpp(std::string input_file, std::string output_file)
{
    // setup IO
    std::ifstream fin(input_file);
    std::ofstream fout(output_file);

    // output header line to CSV
    fout << "WKU,Title,App_Date,Issue_Date,Inventor,Assignee,ICL_Class,References\n";

    // variables holding patent properties
    std::string currID = "",
                title = "",
                appDate = "",
                issDate = "",
                inventor = "",
                assignee = "",
                iclClass = "",
                refs = "",
                currLine;
    bool inPatent = false,
         gotAPD = false;

    // read input file line-by-line and store patent data
    getline(fin, currLine);
    int countPat = 0;
    while (!fin.eof())
    {
        // look at current line
        if (startsWith(currLine, "PATN"))
        {
            // print past patent (unless this is the first one)
            if (inPatent)
            {
                fout << currID
                  << ",\"" << title
                  << "\"," << appDate
                  << "," << issDate
                  << "," << inventor
                  << "," << assignee
                  << "," << iclClass
                  << "," << refs
                  << "\n";
            }
            else inPatent = true;

            // update counter/tracker vars
            countPat++;
            gotAPD = false;
        }
        else if (inPatent && startsWith(currLine, "TTL  "))
        {
            title = currLine.substr(5);
            stripBeginWhitespace(title);
            stripEndWhitespace(title);
        }
        else if (inPatent && startsWith(currLine, "WKU  "))
        {
            currID = currLine.substr(5);
            stripBeginWhitespace(currID);
            stripEndWhitespace(currID);
        }
        else if (inPatent && !gotAPD && startsWith(currLine, "APD  "))
        {
            gotAPD = true;
            appDate = currLine.substr(5);
            stripBeginWhitespace(appDate);
            stripEndWhitespace(appDate);
        }

        // read next line
        getline(fin, currLine);
    }

    // output details of last patent
    fout << currID
         << ",\"" << title
         << "\"," << appDate
         << "," << issDate
         << "," << inventor
         << "," << assignee
         << "," << iclClass
         << "," << refs
         << "\n";

    // close IO
    fin.close();
    fout.close();

    // return number of patents
    return countPat;
}
