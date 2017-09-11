/*
 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file   ObjectParser.h
/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @date   Jun 2012

#include "eckit/value/Value.h"
#include "eckit/parser/ObjectParser.h"
#include "eckit/utils/Translator.h"

namespace eckit {


ObjectParser::~ObjectParser() {
}

//----------------------------------------------------------------------------------------------------------------------

Value ObjectParser::parseTrue()
{
    consume("true");
    return Value(true);
}

Value ObjectParser::parseFalse()
{
    consume("false");
    return Value(false);
}

Value ObjectParser::parseNull()
{
    consume("null");
    return Value();
}

Value ObjectParser::parseNumber()
{
    bool real = false;
    bool string = false;

    std::string s;
    char c = next();
    if (c == '-') {
        s += c;
        c = next();
    }

    switch (c) {
    case '0': s += c; break;
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        s += c;
        while (isdigit(peek())) {
            s += next();
        }
        break;
    default:
        throw StreamParser::Error(std::string("ObjectParser::parseNumber invalid char '") + c + "'");
        break;
    }

    if (peek() == '.') {
        real = true;
        s += next();
        c = next();
        if (!isdigit(c))
            throw StreamParser::Error(std::string("ObjectParser::parseNumber invalid char '") + c + "'");
        s += c;
        while (isdigit(peek())) {
            s += next();
        }
    }


    c = peek();
    if (c == 'e' || c == 'E') {
        real = true;
        s += next();

        c = next();
        if (c == '-' || c == '+')
        {
            s += c;
            c = next();
        }

        if (!isdigit(c))
            throw StreamParser::Error(std::string("ObjectParser::parseNumber invalid char '") + c + "'");
        s += c;
        while (isdigit(peek())) {
            s += next();
        }

    }

    if (string) {
        return Value(s);
    }

    if (real) {
        double d = Translator<std::string, double>()(s);
        return Value(d);
    }
    else
    {
        long long d = Translator<std::string, long long>()(s);
        return Value(d);
    }
}

Value ObjectParser::parseString(char quote)
{
    consume(quote);
    std::string s;
    for (;;)
    {
        char c = next(true);
        if (c == '\\')
        {
            c = next(true);
            switch (c) {

            case '\\':
                s += '\\';
                break;

            case '/':
                s += '/';
                break;

            case 'b':
                s += '\b';
                break;

            case 'f':
                s += '\f';
                break;

            case 'n':
                s += '\n';
                break;

            case 'r':
                s += '\r';
                break;

            case 't':
                s += '\t';
                break;

            case 'u':
                throw StreamParser::Error(std::string("ObjectParser::parseString \\uXXXX format not supported"));
                break;

            default:
                if (c == quote) {
                    s += c;
                }
                else {
                    throw StreamParser::Error(std::string("ObjectParser::parseString invalid escaped char '") + c + "'");
                }
                break;
            }
        }
        else
        {
            if (c == quote)
            {
                return Value(s);
            }
            s += c;
        }

    }

}

void ObjectParser::parseKeyValue(std::map<Value, Value>& m)
{
    Value k = parseString();
    consume(':');
    Value v = parseValue();

    m[k] = v;
}

Value ObjectParser::parseObject()
{
    consume("{");
    char c = peek();
    if (c == '}')
    {
        consume(c);
        return Value::makeMap();
    }

    std::map<Value, Value> m;

    for (;;) {

        parseKeyValue(m);

        char c = peek();
        if (c == '}')
        {
            consume(c);
            return Value::makeMap(m);
        }

        consume(',');

    }
}

Value ObjectParser::parseArray()
{
    consume("[");
    char c = peek();
    if (c == ']')
    {
        consume(c);
        //cout << "ObjectParser::parseArray <== " << std::endl;;
        return Value::makeList();
    }

    std::vector<Value> l;
    for (;;) {

        l.push_back(parseValue());

        char c = peek();
        if (c == ']')
        {
            consume(c);
            //cout << "ObjectParser::parseArray <== " << std::endl;;
            return Value::makeList(l);
        }

        consume(',');

    }
}


Value ObjectParser::parseJSON()
{
    char c = peek();
    switch (c)
    {

    case 't': return parseTrue(); break;
    case 'f': return parseFalse(); break;
    case 'n': return parseNull(); break;
    case '{': return parseObject(); break;
    case '[': return parseArray(); break;
    case '\"': return parseString(); break;

    case '-': return parseNumber(); break;
    case '0': return parseNumber(); break;
    case '1': return parseNumber(); break;
    case '2': return parseNumber(); break;
    case '3': return parseNumber(); break;
    case '4': return parseNumber(); break;
    case '5': return parseNumber(); break;
    case '6': return parseNumber(); break;
    case '7': return parseNumber(); break;
    case '8': return parseNumber(); break;
    case '9': return parseNumber(); break;

    default:
        throw StreamParser::Error(std::string("YAMLParser::parseValue unexpected char '") + c + "'");
        break;
    }
}


ObjectParser::ObjectParser(std::istream &in, bool comments):
    StreamParser(in, comments)
{
}

Value ObjectParser::parse()
{
    Value v = parseValue();
    char c = peek();
    if (c != 0)
        throw StreamParser::Error(std::string("ObjectParser::parse extra char '") + c + "'");
    return v;

}

//----------------------------------------------------------------------------------------------------------------------

} // namespace eckit
