# mustache

[![Erlang CI](https://github.com/Eptwalabha/mustache/actions/workflows/erlang.yml/badge.svg)](https://github.com/Eptwalabha/mustache/actions/workflows/erlang.yml)  
Current version: `v0.0.1`

A library to render [Mustach](https://mustache.github.io/) templates.  
It complies with the [Mustache's manual](https://mustache.github.io/mustache.5.html) and [specs](#specs).

> :warning: While all major features are present, this library is still under development. The following have yet to be added:
> - fetch template & partials from file
> - inheritancy

## Usage

``` erlang
> mustache:render("Hello {{wut}}!", #{ wut => "world" }).
"Hello world!"
> mustache:render(<<"Hello {{wut}}!">>, [{ wut, "world" }]).
"Hello world!"
```

## Sections and non-empty lists (and Strings)

The section **Non-Empty Lists** of the [manual](https://mustache.github.io/mustache.5.html#Sections), states that a section should be displayed as many time as there's element in the list.  
The problem is that erlang's strings [are lists](https://learnyousomeerlang.com/starting-out-for-real#highlighter_829076), which means that the following code will display as such:
``` erlang
> Data = #{ string => "hello" }.
> mustache:render("{{#string}}oups!{{/string}}", Data).
"oups!oups!oups!oups!oups!"
```
Use binary to avoid this issue:
``` erlang
> Data = #{ string => <<"hello">> }.
> mustache:render("{{#string}}ok{{/string}}", Data).
"ok"
```

## Sections and Lambdas
``` erlang
> Fun = fun (Template) ->
                % in here Template = "hello {{name}}",
                "<strong>" ++ Template ++ "</strong>"
        end.
> Map = #{ name => "Tom", lambda => Fun }.
> mustache:render("{{#lambda}}hello {{name}}{{/lambda}}", Map).
"<strong>hello Tom</strong>"
```

## Comments
Tags that begin with a bang (`!`) won't be displayed:
``` erlang
> mustache:render("Hello{{! won't be displayed }}!").
"Hello!"
```

## Partials
Tags that begin with `>` will include external template or "partials".

`mustache:render` takes a third arguments for partials:
``` erlang
> Template = "ducks:\n{{#ducks}}{{>item}}{{/ducks}}".
> Map = #{ ducks => ["Huey", "Dewey", "Louie"] }.
> Partials = #{ item => "- {{.}}\n" }.
> mustache:render(Template, Map, Partials).
"ducks:\n- Huey\n- Dewey\n- Louie\n"
```

### From file
If not given to `render`, Partials will be directly fetched from file  
> This feature is comming soon

## Specs
This library complies with the following specs:

- [x] comments
- [x] delimiters
- [x] dynamic-names \*
- [ ] inheritance
- [x] interpolation
- [x] inverted
- [x] lambdas
- [x] partials \*
- [x] sections

\* : does not pad partials with spaces as requiered by the spec 
## Copyright

This repo is under the MIT's license
