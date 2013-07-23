===================
Style guide for git
===================

:Precis: How to use git in a software project
:Authors: Andrew Lincoln Burrow
:Copyright: 2013 Andrew Lincoln Burrow
:License:
    Permission is granted to copy, distribute and/or modify this
    document under the terms of the GNU Free Documentation License,
    Version 1.3 or any later version published by the Free Software
    Foundation; with no Invariant Sections, no Front-Cover Texts, and no
    Back-Cover Texts.

    The latest version of this license is in
    http://www.gnu.org/copyleft/fdl.html

---------
XXX To Do
---------

- Determine how to appropriately cite the git-flow work where most of
  this originated

- Collect references for sections containing advice

- Complete instructions on merging a feature branch into the development
  branch, and creating a release

------------
Introduction
------------

XXX Describe the problems being solved, and the source of this work.  It
contains code fragments for the command line.  It includes suggestions
for the choice of branch and tag names, and the markup for sections,
identifiers, literals, and example code, and relates these suggestions
to the generation of software documentation.

XXX Not a manual, but does contain reminders of the less frequently used
commands

The best reference for XXX is `Reference for XXX`_.  Also of use is the
`Another place for XXX`_.

.. _Reference for XXX: \
   http://docutils.sourceforge.net/docs/user/rst/quickref.html
.. _Another place for XXX: \
   http://docutils.sourceforge.net/rst.html

--------
Strategy
--------

XXX Mention git-flow

-----
Style
-----

Naming conventions
==================

XXX How we name branches and tags

Commit messages
===============

- Make the first line count.  Write in the imperative

- Markup in order to improve readability using a small subset of a
  popular structured text format: Markdown, reStructuredText, or Org-Mode

- Suggested markup elements: strong, emphasis, code, ordered and
  unordered lists, and definition lists

- Break lines to fit a 72 character line length

-------
Process
-------

XXX The steps organised by scenario, and supplied with fragments that
can be cut and pasted to the command line.  Use of environment variables

Setup of the remote repository
------------------------------

- Identify the development branch.  Follow the naming convention, as the
  branch will have been created for you::

    REPO_BASENAME="datapower"
    REPO_URL="git@github.com:albcorp/AlbLaTeX.git"
    MASTER="master"
    DEVELOP="develop"

- XXX Whitespace rules enforced by the remote

Initial setup
-------------

- Setup the config for this project.  These may be global if they do not
  conflict with other projects::

    git config XXX

- Clone remote repo::

    git clone $REPO_URL

Query the status of the remote repository
-----------------------------------------

XXX For queries about remote status::

  git remote show origin

Query the status of the local repository
----------------------------------------

XXX For queries about local status, check the status and the log::

  git status
  git log --name-status
  git show

Setup a feature branch
----------------------

- Identify the feature branch::

    FEATURE="feature/git-stylesheet"

- Create the local feature branch from a copy of the development branch,
  and switch to this new branch::

    git checkout $DEVELOP
    git pull
    git checkout -b $FEATURE $DEVELOP

- Push the local feature branch to remote, and set the default upstream
  for the local feature branch::

    git push origin $FEATURE
    git branch --set-upstream-to=origin/$FEATURE $FEATURE

  After this, the branch is a feature branch that can be shared between
  developers.

Work on a feature branch
------------------------

Once a local feature branch is established ... XXX

-  For all future updates from remote, pull remote commits::

    git pull

- For all future updates to remote, push commits::

    git push

- For all future local changes, commit to the local dev branch::

    git add ${FILENAME}
    git commit

- In case edits are to be adandoned, you may wish to discard the
  changes.

  To revert a single file ={filename}=, execute::

    git checkout {filename}

  To revert the entire repository, execute::

    git checkout --force HEAD

- In case a commit was premature, you may wish to redo the commit to
  include additional edits.  Edit the files to make the corrections,
  stage the new edits, and then amend the commit.  Execute::

    git commit --amend

- In case a commit contains substantial faults, but the source files are
  to be preserved, you may wish to discard the commit but not the
  changes.  Execute::

    git reset HEAD~1

- In case a commit is entirely compromised, you may wish to discard the
  commit and the changes that it contains.  One common case is a merge
  that is found to be wrong.  Execute::

    git reset --hard HEAD~1


Merge development branch
------------------------

p. Check out and switch to major feature/sprint development branch
i. git checkout ${SPRINT}
q. Merge feature branch
i. git merge --no-ff ${BRANCH}
r. Commit locally
i. git commit –a –m ‘your comment here’
s. Update remote
i. git push

Delete a local feature branch
-----------------------------

After you have merged the feature branch into the development branch,
and pushed the two branches to the remote, you may wish to clean up your
local repository.  You can delete the local feature branch, which will
break the connection to the remote.

- Check out the development branch::

    git checkout ${DEVELOP}

- Delete the local feature branch::

    git branch -d ${FEATURE}
    unset FEATURE

Delete a remote feature branch
------------------------------

In some cases, you might also wish to delete the remote copy of the
feature branch.  For example, if you used the wrong name when you
created and pushed the local feature branch.

- Delete the local feature branch as described above

- Delete the remote development branch::

    git push origin --delete ${FEATURE}


Setup release branch
--------------------

XXX To do

Branch naming conventions
-------------------------

XXX Explain and reference the conventions on version numbering that we
are using

=master=
  Trunk

=develop=
  Project development branch

=release-{major}.{minor}.{patch}=
  Release branches where XXX ...

=feature/{feature_phrase}=
  Feature development branches where XXX ...

