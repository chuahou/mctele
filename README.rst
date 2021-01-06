######
mcjoin
######

|License: MIT| |Build status|

A simple Telegram bot that uses the `Minecraft Query protocol
<https://wiki.vg/Query>`_ to get the list of players on a server, and
sends changes in the list to a Telegram chat.

Usage
=====

To use, simply run the built executable with the following environment
variables set:

* ``MCJOIN_BOT_TOKEN``: Telegram bot token
* ``MCJOIN_CHAT_ID``: Chat ID to send updates to
* ``MCJOIN_SERVER_ADDR``: (optional) Address with port to query (e.g.
  ``192.168.1.1:25565``. Defaults to ``127.0.0.1:25565``.
* ``MCJOIN_QUERY_INTERVAL``: (optional) Interval in seconds between each
  query to make. Defaults to 15 seconds.

Do not forget to set in the ``server.properties``::

	enable-query=true
	query.port=<something>

.. |License: MIT| image:: https://img.shields.io/badge/License-MIT-yellow.svg
	:target: https://opensource.org/licenses/MIT

.. |Build status| image:: https://github.com/chuahou/mcjoin/workflows/nix%20test/badge.svg?branch=master
