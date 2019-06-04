Features:
*Simple query language for extracting data.
*Stored data is bound to a defined schema that defines the name and type of the data-columns
*Invalid format or type of stored data wil trigger an error
*Syntax checking for the query
*Sending a column trough one or more pipe-functions
*Pretty printing of output with fixed width for the columns and headers

Example of query:
#Comments are supported
from employee as e, department as d #and can also be at end of line
merge e and d using depID #merging of tables using a common column(has to match type)
#prints specific columns. named renames the column-header.
# supports piping columns though one or more pipe-functions
output merged.name named ansattnavn, merged.depName -> lower -> trim, merged.salary -> numSep

Usage:
./install for compilation 

./fbase queryFile for running the program 
An example-query with the filename exampleQuery is provided.
This query uses the table emp and dep

./evalExpr expression(as string) for evaluating expressions
When typing expressions, alwas use parentheses for negative values

Dependencies:
*polyml
