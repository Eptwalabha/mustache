# mustache

[![Erlang CI](https://github.com/Eptwalabha/mustache/actions/workflows/erlang.yml/badge.svg)](https://github.com/Eptwalabha/mustache/actions/workflows/erlang.yml)

A library to render [Mustach](https://mustache.github.io/) templates.  
It tries to comply with the [Mustache's manual](https://mustache.github.io/mustache.5.html) as close as possible.

> :warning: This library is still a WIP, the following features have yet to be developped:
> - fetch template & partials from file
> - allow [different delimiter](https://mustache.github.io/mustache.5.html#Set-Delimiter)
> - accept [unicode:chardata()](https://www.erlang.org/doc/man/unicode.html#type-chardata) as input

## Usage

``` erlang
> mustache:render("Hello {{wut}}!", [{wut, "world"}]).
"Hello world!"
```

## Sections and non-empty lists (and Strings)

The section **Non-Empty Lists** of the [manual](https://mustache.github.io/mustache.5.html#Sections), states that a section should be displayed as many time as there's element in the list.  
The problem is that erlang's strings [are lists](https://learnyousomeerlang.com/starting-out-for-real#highlighter_829076), which means that the following code will display as such:
``` erlang
> mustache:render("{{#string}}oups!{{/string}}", [{string, "hello"}]).
"oups!oups!oups!oups!oups!"
```
To avoid this kind of problem, use binary instead:
``` erlang
> mustache:render("{{#string}}ok{{/string}}", [{string, <<"hello">>}]).
"ok"
```

## Sections and Lambdas
``` erlang
> Fun = fun (Template, Render) ->
                New_template = "<strong>" ++ Template ++ "</strong>",
                Render(New_template)
        end.
> Params = [{name, "Tom"}, {lambda, Fun}].
> mustache:render("{{#lambda}}hello {{name}}{{/lambda}}", Params).
"<strong>hello Tom</strong>"
```

## Comments
Tags that begin with a bang (`!`) won't be displayed:
``` erlang
> mustache:render("Hello{{! won't be displayed }}!").
"Hello!"
```

## Partials
Tags that begin with `>` will include external template.

### Using `Params`
Partial can be stored in the Parameters used to render the template like so:
``` erlang
> Params = [{ducks, ["Huey", "Dewey", "Louie"]},
            {item, "- {{.}}\n"}].
> Template = "ducks:\n{{#ducks}}{{>item}}{{/ducks}}".
> mustache:render(Template, Params).
"ducks:\n- Huey\n- Dewey\n- Louie\n"
```

### Using files
> Comming soon

## Copyright

This repo is under the MIT's license
