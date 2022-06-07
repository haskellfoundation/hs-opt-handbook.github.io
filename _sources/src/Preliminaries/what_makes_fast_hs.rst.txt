.. _sec-four-horsemen:

The Four Horsemen of Bad Haskell Performance
============================================

.. todo::
   - High level easy to understand enumeration of enemies of speed.
   - Start with abstract and generalities of slow programs in general, e.g., link chasing
   - Then move to haskell in particular

This section answers the question ``What makes Haskell programs fast?``. In
general, the enemy of high performance is pointer chasing. However,
