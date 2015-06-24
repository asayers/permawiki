
# `PermaWiki` - a wiki with content-based addressing

There are two kinds of object in this wiki: pages, and page versions. A page
version is an immutable piece of content, and is addressed by its hash. A page
is a list of references to page versions, to which new versions can be
appended.

## Motivation

Stable addressing. This scheme allows external entities to link to a page,
knowing that their visitors will be shown to exactly the same content which
they intended to reference.

This addressing scheme even comes with a built-in guarantee. Given a link to a
PermaWiki page, you can verify yourself that the page you recieved is the page
you were intended to see.

## To-Do

- print hashes in base64
- use blaze-html for templating
- add page ID to page versions
- show header in /perma/ pages if there's a newer version
- associate names with PageIds
- use a database
