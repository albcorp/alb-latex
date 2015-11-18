===================
Style guide for git
===================

:Precis: How to use git branches in albcorp projects
:Authors: Andrew Lincoln Burrow
:Contact: albcorp@gmail.com
:Copyright: 2014-2015 Andrew Lincoln Burrow
:License:
    Permission is granted to copy, distribute and/or modify this
    document under the terms of the GNU Free Documentation License,
    Version 1.3 or any later version published by the Free Software
    Foundation; with no Invariant Sections, no Front-Cover Texts, and no
    Back-Cover Texts.

    The latest version of this license is in
    http://www.gnu.org/copyleft/fdl.html

------------
Introduction
------------

The following style guide presents options for the use of git in my
personal projects.  It includes guidance on three workflows.  A project
should begin with the first and simplest workflow.  This allows the
initial sketch to quickly develop.  The project can then switch to a
more sophisticated workflow as the need arises.  The following workflows
draw on material presented in the `Atlassian guide on git workflows`_.

- The `centralised workflow`_ is the simplest possible workflow.  It is
  the best way to quickly start a new project.  The workflow takes steps
  to ensure an easy-to-read linear history.  However, it restricts a
  developer to working on a single issue at a time
- The `feature vine workflow`_ extends the `centralised workflow`_ to
  allow a developer to work on multiple issues in parallel.  It does
  this by organising development into a separate feature branch per
  issue.  The workflow preserves the easy-to-read linear history.
  However, it is impractical when many feature branches are developed in
  parallel and integrated on mass
- The `feature branch workflow`_ extends the `feature vine workflow`_ to
  accommodate concurrent development across a team supported by
  automated testing.  It ensures that feature branches are explicitly
  represented in the history graph.  This allows open feature branches
  to be assembled into a release candidate and tested.  However, the
  history must be interpretted using the merge commits as markers to
  determine the source of changes

The workflows are detailed in the sections that follow.  Each workflow
is presented in a self contained description that is intended to be
included in the top-level README of a project.

.. _Atlassian guide on git workflows:
   https://www.atlassian.com/git/workflows

--------------------
Centralised workflow
--------------------

This project uses the *centralised workflow*.  This simple workflow does
not require the use of branches.  Instead, it uses a central, remote
repository and a local repository on each developer's workstation.  It
is the best way to quickly start a new project.  However, it restricts a
developer to working on a single issue at a time.

The workflow ensures a linear history with easy to follow commit
messages.  Each developer must rebase their local repository before
pushing to the remote repository.  Each commit message must be labelled
by the issue number that it addresses.  As a consequence of these
stipulations, each commit corresponds to an issue, and the issue is
immediately recognisable by reading the commit message.

An excellent descriptions of the `centralised workflow`_ is provided by
Atlassian.  Please read that article for additional background.  The
remainder of this section provides details specific to this repository,
including commit message conventions, and example command invocations.

.. _centralised workflow:
   https://www.atlassian.com/git/tutorials/comparing-workflows/centralized-workflow

The origin repository
=====================

This workflow depends on a single remote repository centralising the
interactions between developers.  Git is a distributed version control
system: every working copy is itself a git repository, and every git
repository can communicate with others.  However, every git repository
records a reference to the repository from which it was cloned.  This is
the ``origin`` remote.  Since each developer clones their local
repository from the same central repository, the remote known as
``origin`` is the same in every developer's local repository.  The
centralised workflow consists in fetching from and pushing to the
``origin`` remote repository.

The master branch
=================

No additional branches are required in the centralised workflow.
Instead, the ``origin`` repository and each local copy all use the
default ``master`` branch.  Workflows that employ multiple branches
typically stipulate that the ``master`` must only contain
production-ready code.  The centralised workflow relaxes this to
stipulate that the ``master`` branch **on** the ``origin`` remote must
only contain production-ready code.  The centralised workflow consists
in developing and testing improvements in the local repositories until
they are production-ready.

Commit comments
===============

Ensure that each commit enjoys a comment meeting the following
conditions.  This ensures that history can be easily browsed without
requiring specialist tools to unravel the history.

#. Prefix the first line of each comment by the ``#`` character,
   followed by the issue number, a colon, and a space.  This format is
   automatically converted to a link to the *GitHub* issue. For example,
   if working on issue ``7`` the first line of a comment might be::

      #7: Git ignore the Vagrant support files

#. Write the first line of each comment as a concise command, namely in
   the *imperative*.  The most important words appear close to the start
   of an imperative sentence, and this form uses the simplest verb
   tenses.  At times this will seem unnatural, and you will be tempted
   to use the past tense, but these constructions add no further
   information and often become passive or woolly.

   The following are examples of imperative sentences appropriate to the
   first line of a commit comment.

   - Add configuration for VMWare Fusion
   - Remove extraneous comments
   - Git ignore the Vagrant support files

#. Use only the inline formatting common to reStructuredText and
   Markdown, namely ``*`` for emphasis around titles, a single backtick
   around identifiers and filenames

Git configuration
=================

Run the following commands to set the configuration for this repository.
If you wish, you can do this once for all repositories by substituting
``--global`` for ``--local``::

   git config --local --add core.commentchar %
   git config --local push.default simple
   git config --local pull.ff only
   git config --local merge.ff only

The first setting allows the use of the ``#`` character in the commit
messages.  The second asserts the simple default for when no further
arguments are passed to ``git push``.  This is the default since Git
version 2.0.  The third and fourth prevent merges when a rebase is
required.  It will also prevent an inadvertent merge if you have resumed
ownership of a feature branch that has had its history modified.  In
this second case, you need to discard the local branch and re-checkout
the updated branch.

Git command examples
====================

The following subsections provide complete working examples for the key
workflow actions.  These are the commands that would be issued on a
local repository by a developer.  Context specific aspects are captured
as environment variables.  Take care to update the environment variables
in the script fragments to reflect your specific context.

Rebase the local repository
---------------------------

Rebase your local repository to include changes committed to the remote
repository since the local repository was last rebased.  This involves
two steps: first, fetch the current state of the remote; and second,
rebase the local repository to include the new commits::

   git fetch
   git rebase origin/master

Carefully follow the prompts that git provides whenever interaction is
required.  The rebase will either complete automatically, or some of
your commits will conflict with commits from the remote.  In the second
case, git will pause the rebase operation, and prompt you to resolve the
conflicts.  See `Basic merge conflicts`_ on `git-scm.com`_ for a short
tutorial.

.. _git-scm.com:
   http://git-scm.com/
.. _Basic merge conflicts:
   http://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging#Basic-Merge-Conflicts

**IF** the remote master branch has not advanced since the local was
last rebased or pushed, **THEN** the rebase will be a simple
no-operation.

**IF** the rebase operation goes pear shaped before you finish, **THEN**
abort the rebase and retry::

   git rebase --abort

**IF** you complete the rebase **AND** are unhappy with the result,
**THEN** reset the rebase.  This works if you have not performed another
merge, rebase, or reset, because each of these commands stores a backup
under ``ORIG_HEAD``.  Consider first creating a full back up of the
repository, so that you can get expert help rolling back, e.g., using
``tar`` or ``cp --archive --link``.  Then, perform a hard reset to the
backup reference::

   git reset --hard ORIG_HEAD

Push local commits to the remote repository
-------------------------------------------

Given the local repository is up to date with the remote, the push will
simply fast-forward the remote::

   git push

**IF** the remote master branch has advanced since the local was last
rebased or pushed, **THEN** the push operation will fail.  Perform
another rebase.

Tag a release
-------------

Tag the remote master to indicate a version ready for release.  For
example, if the most recent push has advanced the code to version
``0.2``, tag the commit and push the tag to the remote::

   VERSION="v0.2"
   git tag --annotate --message="Release ${VERSION}" ${VERSION}
   git push --tags

---------------------
Feature vine workflow
---------------------

This project uses the *feature vine workflow*.  This is the descriptive
name of a feature branch workflow that requires each feature branch be
rebased before merging into the master branch.  This relatively simple
workflow is appropriate when there is a modest amount of concurrent
development being done, but releases correspond to the addition of a
single feature branch to the master branch.  The stipulation that
feature branches be rebased before performing a fast-forward merge
ensures a linear history.  The further stipulation that each commit be
labelled by the issue number ensures the issue responsible for each line
of code can be quickly and easily determined without reference to
branches.

An excellent descriptions of the `feature branch workflow`_ is provided
by Atlassian.  Please read that article for additional background.  The
remainder of this section provides details specific to this repository,
including branch naming conventions, and provides example command
invocations.

.. _feature branch workflow:
   https://www.atlassian.com/git/workflows#!workflow-feature-branch

Feature branches
================

This workflow uses feature branches to store development work.  It is
expected that feature branches are stored on the remote repository.
This practice complements the use of `pull requests`_ to signal code
that is ready for review before being merged into the master branch.
Pushing feature branches to the remote repository also provides an
additional level of backup.  Since work on a feature branch is carried
out by a single developer, and since the pull request clear signals the
point at which the feature branch becomes of interest to others, it is
relatively safe to rewrite history on the feature branch to improve its
clarity.

It is **very** important that *GitHub* issue assignment is used as the
gatekeeper to working on a feature branch.  The owner of the feature
branch is the person who is assigned the issue.  They will modify
history when they rebase the feature branch.  Before handover, the
current owner must push all changes to the remote and delete their local
copy.  At handover, the new owner must delete their local copy of the
feature branch before fetching and checking out the most recent state of
the feature branch.

.. _pull requests:
   https://help.github.com/articles/using-pull-requests/

Commit comments
===============

Ensure that each commit enjoys a comment meeting the following
conditions.  This ensures that history can be easily browsed without
requiring specialist tools to unravel the history.

#. Prefix the first line of each comment by the ``#`` character,
   followed by the issue number, a colon, and a space.  This format is
   automatically converted to a link to the *GitHub* issue. For example,
   if working on issue ``7`` the first line of a comment might be::

      #7: Git ignore the Vagrant support files

#. Write the first line of each comment as a concise command, namely in
   the *imperative*.  The most important words appear close to the start
   of an imperative sentence, and this form uses the simplest verb
   tenses.  At times this will seem unnatural, and you will be tempted
   to use the past tense, but these constructions add no further
   information and often become passive or woolly.

   The following are examples of imperative sentences appropriate to the
   first line of a commit comment.

   - Add configuration for VMWare Fusion
   - Remove extraneous comments
   - Git ignore the Vagrant support files

#. Use only the inline formatting common to reStructuredText and
   Markdown, namely ``*`` for emphasis around titles, a single backtick
   around identifiers and filenames

Git configuration
=================

Run the following commands to set the configuration for this repository.
If you wish, you can do this once for all repositories by substituting
``--global`` for ``--local``::

   git config --local --add core.commentchar %
   git config --local push.default simple
   git config --local pull.ff only
   git config --local merge.ff only

The first setting allows the use of the ``#`` character in the commit
messages.  The second asserts the simple default for when no further
arguments are passed to ``git push``.  This is the default since Git
version 2.0.  The third and fourth prevent merges when a rebase is
required.  It will also prevent an inadvertent merge if you have resumed
ownership of a feature branch that has had its history modified.  In
this second case, you need to discard the local branch and re-checkout
the updated branch.

Git command examples
====================

The following subsections provide complete working examples for the key
workflow actions.  These are the commands that would be issued on a
local repository by a developer.  Note the following.

- Context specific aspects are captured as environment variables.  Take
  care to update the environment variables in the script fragments to
  reflect your specific context
- The script fragments assume that all local commits have been pushed to
  the remote.  This is not an obvious assumption.  In this workflow the
  remote plays the role of centralised store of feature branches.  Other
  workflows expect the developer to perform certain operations on
  commits and branches held entirely in their local repository

Create a new feature branch
---------------------------

Create a new feature branch when you first begin work on an issue.  If
the issue has be reassigned to you, see instead `Take over an existing
feature branch`_.

Let the work be on issue ``7``.  Checkout the ``master`` branch; ensure
the branch is up to date with the remote repository; create the feature
branch, and push the new feature branch to the repository::

   ISSUE="issue/7"
   git checkout master
   git pull
   git checkout -b ${ISSUE}
   git push --set-upstream origin ${ISSUE}

Release an existing feature branch
----------------------------------

Release an existing feature branch when you stop working on the issue,
or the issue is reassigned to another developer.  You **must** do this,
because each developer is free to rebase the feature branch that they
are working on, and your local copy will become dangerously out of sync
with the remote copy.

Let the work be on issue ``7``.  Checkout the feature branch; and check
the status of the branch::

   ISSUE="issue/7"
   git checkout ${ISSUE}
   git status

If there are outstanding changes, you must decide what to do with them
based on their quality:

- Commit high quality changes with an extensive comments, so that the
  next developer can make sense of them
- Stash interesting but unpolished changed cases, so that you can refer
  to them later if required
- Discard all other changes.  One quick trick is to stash the changes,
  and then drop the stash

Once all outstanding changes have been committed, stashed, or discarded,
push the local commits to the remote; switch to the master; and delete
the local copy of the feature branch::

   git push
   git checkout master
   git branch -d ${ISSUE}

Take over an existing feature branch
------------------------------------

Take over an existing feature branch when an issue is reassigned to you.
If the issue has not yet been worked on, see instead `Create a new
feature branch`_.

Let the work be on issue ``7``.  Checkout the feature branch; ensure the
branch is up to date with the remote repository; create the feature
branch, and push the new feature branch to the repository::

   ISSUE="issue/7"
   git checkout ${ISSUE}
   git pull

**IF** you had previously checked out the feature branch **AND** not
followed the instructions in `Release an existing feature branch`_,
**THEN** the pull might fail.  This is as it should be, because the
local repository is configured to reject pulls that do not fast-forward
the local branch.

In this case, you have two choices.  You can delete the local branch,
and lose your changes, or interactively rebase cherry picked commits
from the local branch.  Whether it is worth pursuing the second option
depends on the following factors.

- Have commits been lost?  Commits that you had pushed to the remote,
  are not in fact lost.  Look at the log of your local branch, and note
  the comments and dates.  Now look at the log on the remote branch,
  e.g. ``origin/${ISSUE}``.  Check whether the commits exist, by
  searchin on your name and the dates
- Do the lost commits contain important changes?
- What is the quality of the lost commits?  Are they distinct and well
  commented?  If the lost commits entangle changes, they may be
  difficult to rebase, and might require an initial rebase to split them
  into workable pieces
- How proficient are you with the interactive rebase feature in git?

If in doubt, consult a git expert in your team.

Rebase a feature branch
-----------------------

Rebase your feature branch in preparation for a release.  This allows
the master branch to be *fast-forwarded* to include the commits on the
feature branch, so that a linear history is preserved.  Once you have
rebased your feature branch, you should test the code again to ensure
that conflicts with the new commits from the remote have not caused
bugs.

Let the work be on issue ``7``.  Checkout and update the ``master``
branch, checkout and update the feature branch, and rebase the feature
branch::

   ISSUE="issue/7"
   git checkout master
   git pull
   git checkout ${ISSUE}
   git pull
   git rebase master

**IF** the master branch has not advanced since the feature branch was
created, **THEN** the rebase will be a simple no-operation.

**IF** the rebase operation goes pear shaped before you finish, **THEN**
abandon the rebase::

   git rebase --abort

**IF** you complete the rebase but are unhappy with the result, **THEN**
discard the messed up local feature branch, and checkout a fresh copy
from the remote::

   git checkout master
   git branch -D ${ISSUE}
   git checkout ${ISSUE}

**OTHERWISE** the rebase has gone to plan, so push the new state of the
feature branch up to the remote::

   git push --force

Create a new release
--------------------

To create a new release, merge the rebased and tested feature branch
onto the master branch, and push to the remote.

Let the work be on issue ``7``, and the new version be ``v0.2``.
Checkout and update the feature branch, push the feature branch onto the
remote master branch::

   ISSUE="issue/7"
   VERSION="v0.2"
   git checkout ${ISSUE}
   git pull
   git push origin master

**IF** the push is rejected, **THEN** the remote has been updated since
you rebased.  Simply rebase the feature branch again, and restart this
process, noting that you will need to update the version number.

**OTHERWISE** the release has succeeded: tag the release; push the tag
to the remote; and delete the local and remote copies of the feature
branch::

   git checkout master
   git pull
   git tag --annotate --message="Merge in ${ISSUE} " ${VERSION}
   git push --tags
   git branch -d ${ISSUE}
   git push origin --delete ${ISSUE}

Clean up a feature branch
-------------------------

Clean up the feature branch when an issue is merged.  This is to ensure
that the list of feature branches on the repository reflects only active
issues, rather than completed or abandoned issues.

Let the work be on issue ``7``.  Delete the feature branch on the local
and remote::

   ISSUE="issue/7"
   git checkout master
   git branch -D ${ISSUE}
   git push origin --delete ${ISSUE}

-----------------------
Feature branch workflow
-----------------------

This project uses the *feature branch workflow*.  This workflow is
designed to ensure that feature branches can be selectively combined to
create a release.  This is realised by ensuring the following:

#. feature branches are short lived;
#. every open feature branch shares the same branch point on the master
   branch; and
#. every open feature branch is independent of every other open feature
   branch.

In this case, a release candidate is formed by merging a susbset of the
open feature branches.  If the release candidate passes all tests, then
it can be merged back to the master, the selected feature branches
archived, and the outstanding feature branches rebased.  Otherwise, a
new release candidate can be formed by merging another subset of the
open feature branches.

Several descriptions of this workflow are available online.  It has
recently been described in detail by Adam Dymitruk as the
`branch-per-feature`_ workflow, but it has also previously been
described as the `branch-per-task`_ in the context of other version
control systems.  Note that the term *branch-per-feature* is already
widely used for workflows that do not impose the constraint of sharing
the branch point or ensuring independence of feature branches.

.. _branch-per-feature:
   http://dymitruk.com/blog/2012/02/05/branch-per-feature/
.. _branch-per-task:
   http://codicesoftware.blogspot.com/2010/08/branch-per-task-workflow-explained.html

Feature branches, and release candidates
========================================

The following imperatives provide an initial outline of the workflow.
They cover the relationship between feature branches and the release
process.  The archiving, integration, and testing processes introduce
additional complexities covered in subsequent sections.

- Store only releases on the ``master`` branch

- Branch each feature branch off ``master``, one per issue; and name the
  branch in the form ``issue/N`` where ``N`` is the issue number

- Branch a release candidate branch off ``master``; name the branch in
  the form ``rc/DATE`` branch where ``DATE`` is replaced by the current
  date in ISO 8601 format; and merge the completed feature branches into
  the release candidate branch

- Merge the succesful release canididate branch into ``master``; tag the
  release on the master branch; archive the completed feature branches;
  delete the release branch; and rebase or archive incomplete feature
  branches

Commit comments
===============

Ensure that each commit enjoys a comment meeting the following
conditions.  This ensures that history can be easily browsed without
requiring specialist tools to unravel the history.

#. Prefix the first line of each comment by the ``#`` character,
   followed by the issue number, a colon, and a space.  This format is
   automatically converted to a link to the *GitHub* issue. For example,
   if working on issue ``7`` the first line of a comment might be::

      #7: Git ignore the Vagrant support files

#. Write the first line of each comment as a concise command, namely in
   the *imperative*.  The most important words appear close to the start
   of an imperative sentence, and this form uses the simplest verb
   tenses.  At times this will seem unnatural, and you will be tempted
   to use the past tense, but these constructions add no further
   information and often become passive or woolly.

   The following are examples of imperative sentences appropriate to the
   first line of a commit comment.

   - Add configuration for VMWare Fusion
   - Remove extraneous comments
   - Git ignore the Vagrant support files

#. Use only the inline formatting common to reStructuredText and
   Markdown, namely ``*`` for emphasis around titles, a single backtick
   around identifiers and filenames

Git configuration
=================

Run the following commands to set the configuration for this repository.
If you wish, you can do this once for all repositories by substituting
``--global`` for ``--local``::

   git config --local --add core.commentchar %
   git config --local push.default simple
   git config --local pull.ff only
   git config --local merge.ff only

The first setting allows the use of the ``#`` character in the commit
messages.  The second asserts the simple default for when no further
arguments are passed to ``git push``.  This is the default since Git
version 2.0.  The third and fourth prevent merges when a rebase is
required.  It will also prevent an inadvertent merge if you have resumed
ownership of a feature branch that has had its history modified.  In
this second case, you need to discard the local branch and re-checkout
the updated branch.

Integration and testing
=======================

The risk in leaving integration until the end of the development cycle
is of uncovering complex merge conflicts under time pressure.  To
ameliorate these risks, create and discard exploratory integration
branches throughout the development cycle.  This allows the development
team to discover complex merge conflicts early, to capture conflict
resolutions, and to run regression tests on the conflict resolutions.

Test integration by taking the following steps throughout the
ddevelopment cycle.

- Recreate the integration branch: delete any local and remote
  ``develop`` branch; branch a new ``develop`` branch off ``master``;
  merge all feature branches into ``develop`` and resolve conflicts; and
  push to the remote

- Test new commits for conflicts: pull the current ``develop`` branch
  from the remote repository; merge in the local feature branch; and
  resolve conflicts

If the conflicts indicate the need for a code refactor to
be shared by several feature branches, the development team has the
opportunity to develop this refactor to create a new baseline, and then
rebase each outstanding feature branch from this new baseline.

The framework for testing is yet to be specified.  Use the provided unit
tests.

Git command examples
====================

The following are examples of git commands for a selection of the
actions demanded by the workflow.  Each example demonstrates commands
that would be issued on a local repository by a developer.

Create a new feature branch
---------------------------

For example, create a feature branch for issue number 12.  Checkout the
``master`` branch, ensure the branch is up to date with the remote
repository, create the feature branch, and push the new feature branch
to the repository::

   ISSUE="issue/12"
   git checkout master
   git pull
   git checkout -b ${ISSUE}
   git push --set-upstream origin ${ISSUE}

Create a release candidate
--------------------------

For example, create a release candidate for issues numbered 17 and 19 on
12 June, 2014.  Checkout the ``master`` branch, ensure the branch is up
to date with the remote repository, create the release candidate branch,
and push the new release candidate branch to the repository::

   RC="rc/2014-06-14"
   git checkout master
   git pull
   git checkout -b ${RC}
   git push --set-upstream origin ${RC}

The next commands perform the merges.  The forms presented here use
fetch so that local branches are not endangered.  Therefore, these
merges can be safely performed in a local repository that contains
feature branches that are ahead of the remote repository, namely, they
can safely be performed by a developer.

Fetch the feature branch for issue number 17 from the remote repository,
and merge into the release candidate::

   ISSUE="issue/17"
   git fetch origin ${ISSUE}
   git merge --no-ff origin/${ISSUE}

Fetch the feature branch for issue number 19 from the remote repository,
and merge into the release candidate::

   ISSUE="issue/19"
   git fetch origin ${ISSUE}
   git merge --no-ff origin/${ISSUE}

If there are conflicts during the second merge these will have to be
resolved and then committed.  The use of ``rerere`` is not shown here.

Push the release candidate back to the remote repository::

   git push

Merge a succesful release canididate branch into master
-------------------------------------------------------

For example, merge the release candidate created on 12 June, 2014 to
create version ``0.1``.  Checkout the ``master`` branch, ensure the
branch is up to date with the remote repository, merge in the release
candidate branch, tag the new release, and push the new ``master`` to
the repository::

   RC="rc/2014-06-14"
   VERSION="v0.1"
   git checkout master
   git pull
   git fetch origin ${RC}
   git merge --no-ff origin/${RC}
   git tag --annotate --message="Release ${VERSION}" ${VERSION}
   git push
   git push --tags

See the examples that follow for the steps to archive the completed
feature branches, delete the release branch, and rebase or archive
incomplete feature branches.

Archive a feature branch
------------------------

For example, archive the feature branch for issue number 17, given it
has been merged into version ``0.1``.  Checkout the feature branch,
ensure the branch is up to date with the remote repository, tag the
feature branch and push to the remote, and delete the feature branch on
the local and remote::

   ISSUE="issue/17"
   VERSION="v0.1"
   git checkout ${ISSUE}
   git pull
   git tag --annotate --message="Merged into ${VERSION}" \
       archive/${ISSUE}
   git push --tags
   git checkout master
   git branch -d ${ISSUE}
   git push origin --delete ${ISSUE}

In case the feature branch is abandoned rather than merged, change the
tag command to::

   git tag --annotate --message="Abandoned at ${VERSION}" \
       archive/${ISSUE}

Recreate the integration branch
-------------------------------

For example, create an integration branch for issues numbered 17 and 19.
Checkout the ``master`` branch, ensure the branch is up to date with the
remote repository, create or reset the local ``develop`` branch to
branch off the current ``master``, and forcibly push ``develop`` to the
remote repository (the ``+`` ensures that **only** ``develop`` is
forcibly pushed)::

   git checkout master
   git pull
   git branch --force develop
   git checkout develop
   git push --force --set-upstream origin +develop

The next commands perform the merges.  The forms presented here use
fetch so that local branches are not endangered.  Therefore, these
merges can be safely performed in a local repository that contains
feature branches that are ahead of the remote repository, namely, they
can safely be performed by a developer.

Fetch the feature branch for issue number 17 from the remote repository,
and merge into the integration branch::

   ISSUE="issue/17"
   git fetch origin ${ISSUE}
   git merge --no-ff origin/${ISSUE}

Fetch the feature branch for issue number 19 from the remote repository,
and merge into the integration branch::

   ISSUE="issue/19"
   git fetch origin ${ISSUE}
   git merge --no-ff origin/${ISSUE}

If there are conflicts during the second merge these will have to be
resolved and then committed.  The use of ``rerere`` is not shown here.

Test for conflicts on the integration branch
--------------------------------------------

For example, you are a developper working on issue number 17 and your
feature branch is ahead of the remote.  Testing for conflicts on the
integration branch allows you to see how your commits integrate with the
work of others without pushing to the remote.

Checkout the ``develop`` branch, ensure the branch is up to date with
the remote repository, merge into the integration branch from your local
feature branch::

   ISSUE="issue/17"
   git checkout develop
   git pull
   git merge --no-ff ${ISSUE}

If there are conflicts during the second merge these will have to be
resolved and then committed.  The use of ``rerere`` is not shown here.

It is relatively safe to push the updated integration branch back to the
remote.  The integration branch is periodically rebuilt, so even if your
commit history changes on the local feature branch, there will be no
lasting damage.  Also, by pushing changes back, you provide other
developers early warning of impending conflicts.
