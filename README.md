Kucumberl
=========

* [What is Kucumberl?](#about)
* [Quick start](#starthere)
 * [How can i use kucumberl in my erlang project?] (#kucumberlmyproject)
 * [How can i create my first test?] (#quicktutorial)
* [Author](#author)
* [License](#license)

## What is kucumberl? <a name="about"></a>

A pure-erlang, open-source, implementation of [Cucumber](http://cukes.info)

## Quick start <a name="starthere"></a>

It's easy

    $ git clone https://github.com/openshine/kucumberl
    $ cd kucumberl
    $ make

And if you want to run the kucumberl examples...

    $ make test

or (if you want to see examples with failures)

    $ ./kucumberl

### How can i use kucumberl in my erlang project? <a name="kucumberlmyproject"></a>

You can copy the kucumberl binary generated when you excute 'make' to your project folder. kucumberl is a self-contained Erlang script.

### How can i create my first test? <a name="quicktutorial"></a>

Copy the kucumberl script to your project folder

    $ cp <kucumberl_folder>/kucumberl <myproject_folder>/

Create the test folders.

    $ cd <myproject_folder>
    $ mkdir -p tests/welcome/features/step_definitions

The folder structure is easy to explain
 
     $ tree
     .
     |-- kucumberl                     <- This is your kucumberl script
     `-- tests                         <- This is your test directory
        `-- welcome                    <- This is a directory to store a group of features to test
           `-- features                <- Here goes the feature's files. In other words, the test description.
              |-- step_definitions     <- Here goes the erl files.

Now, we go to create a new feature file to test the "welcome process" of our app. 
A feature file is a description of a scenario in plain text. The format of this file is really easy to read and write:
Given a situation, When something, Then something...
Please, put the next at ./tests/welcome/features/welcome_user.feature

```feature
Feature: Welcome user
  In order to check the welcome process
  As a new user
  I want to check the welcome service in this site

  Scenario: Hello my name is...
    Given I type my name, 'John'
    And I type my surname, 'Doe'
    When I press 'good welcome' button
    Then I see in the screen 'Hello, Mr John Doe'

```

If you save the feature file and execute ./kucumberl you will see something like that.

```
Feature: Welcome user
  Scenario: Hello my name is...
    Given I type my name, 'John'                                 Not implementated
    And I type my surname, 'Doe'                                 Not implementated
    When I press 'good welcome' button                           Not implementated
    Then I see in the screen 'Hello, Mr John Doe'                Not implementated

0 Scenario (0 failed, 0 passed)
4 Steps (0 failed, 0 passed, 0 skipped, 4 not implementated)
```

Yes, you have not implemented the test. 
This is the next step, you have to define in the steps_definitions directory what to do for each step (Given-When-Then).
Now, please, put the next code at ./tests/welcome/features/step_definitions/welcome_user.erl

```erlang
-module(welcome_user).
-export([setup/0, teardown/0, given/3, 'when'/3, then/3]).

-record(state, {name, surname}).

setup() -> #state{}.

given ("I type my name, '(\\w+)'", State, [Name]) ->
    %% Really complex testing code here! :P
    {ok, State#state{name = Name}};

given ("I type my surname, '(\\w+)'", State, [Surname]) ->
    %% Really complex testing code here! :P
    {ok, State#state{surname = Surname}}.

'when' ("I press 'good welcome' button", State, []) ->
    %% Do something :P
    {ok, State}.

then ("I see in the screen '(.+)'", State, [Str]) ->
    Name = State#state.name,
    Surname = State#state.surname,

    %% More complex testing code to generate the "hello" string :P
    GenStr = "Hello, Mr " ++ Name ++ " " ++ Surname,

    case Str =:= GenStr of
        true -> {ok, State};
	    false -> {failed, "Wow!"}
    end.

teardown() -> ok.
```

And now, execute ./kucumberl and you will see something like that.

```
$ ./kucumberl 
Feature: Welcome user
  Scenario: Hello my name is...
    Given I type my name, 'John'                                 OK
    And I type my surname, 'Doe'                                 OK
    When I press 'good welcome' button                           OK
    Then I see in the screen 'Hello, Mr John Doe'                OK

1 Scenario (0 failed, 1 passed)
4 Steps (0 failed, 4 passed, 0 skipped, 0 not implementated)
```

If you want to check more complex examples, go [here](http://github.com/openshine/kucumberl/tree/master/examples).

## Author <a name="author"></a>

This is an [openshine](http://www.openshine.com) project developed by:
  * Roberto Majadas (roberto.majadas at openshine.com)

## License <a name="license"> ##

Copyright 2012 OpenShine S.L.

Licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

