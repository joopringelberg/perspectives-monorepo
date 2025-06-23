# Release History

## v3.0.0

This release represents a significant milestone for the Perspectives project. It is the first version that we think can be used in a real setting (Technological Readiness Level 7: System prototype demonstration in operational environment). We express and celebrate that with a first major version number (for monorepo-technical reasons that had to be 3).


### Milestones reached
MyContexts Version v3.0.0 delivers the following milestones in the project _Perspectives: Making Models_ as supported by **NLnet**:

* Generic GUI improvements (Milestone 7)
* Custom screens for system functionality (Milestone 8)

### Major Changes


#### Major Functional changes
* **Model translation**. Types in a model text figure largely in the automatically generated screens of the GUI. Up till now, the literal strings occurring in the model text had to be used. We now have a translation system in place that

  * allows for translation in multiple languages;
  * reads files with a dedicated yaml syntax;
  * allows to translate type names, markdown texts, notifications, action names and titles;
  * round-trips from model to generated translation file to edited and uploaded translation file and finally integrates model changes back into that translation file.

* **A new mobile first user interface**. We designed a new concept for the user interface. Every context is now presented in terms of

  * **who** plays a role in it?
  * **what** is it about?
  * **where** can I go to from here?

  These new screens are generated automatically and can be mixed with defined screens in the model. The previously used language to model screens can still be used to produce the What section. **Chat** automatically shows up on the Who section. Roles can be removed by swiping.

  A separate design caters for laptop/tablet.

* **multi-role clipboard**. This proves to be invaluable for the mobile environment.

* **model-based settings**. Each model can now contribute settings to the Settings panel available from the global menu. Language choice is available there. Turning on notifications is in the Settings panel, too.

* **More robust database handling**. When MyContexts relies on IndexedDB (behind the Pouchdb API) it faces a number of issues. Views can be out of sync and we now have a mechanism to detect and repair this in important situations (like when a reference to a non-existing resource is found). Also, we compact databases on a regular basis, to keep their sizes in check. The post database tended to grow quickly in size as every transaction is stored there temporarily. We also remove 'tombstone' docs from the post database.

* **Restore context from peer data**. Being a highly distributed application, MyContexts always contained the promise of restoring lost or damaged data from peers. We now have a first version of that functionality in place. A user can restore a context for a peer. This requires an out-of-band request from that peer. In a future development we'll automate that, e.g. when a reference to a (locally) non-existing resource is found.

* **Makeover of all models and their screens**. All (system) models now have screens defined for them for the new WWW screen. In many we have texts that guide the user. System models are hidden from the Apps menu by default (governed by a setting).

#### Changes to the development environment
* **Purescript upgrade**: Moved from Purescript 14.x to 15.5.
* **Typescript**: all javascript sources making up perspectives-react and mycontexts have been rewritten in typescript.
* **Git management**: All packages are now combined in one [monorepo](https://github.com/joopringelberg/perspectives-monorepo).
* **Unified versioning**: All packages now share version 3.0.0. we now use lerna and conventional commits to roll out versions and release history changes.
* **Automatic updates**: a non-functional change bridging dev and op: updates are downloaded automatically, the user is notified and can decide to apply it immediately or wait till the next startup.
* **Purescript source map files** are now included for debugging.
* **Introduction of vite** for rapid local development. The local Vite runs on https, too.
* **Rollup instead of Webpack**. Rollup is a faster, more modern tool for producing web bundles. It also integrates tightly with Vite.

### Minor changes
Out of 420 commits we pick some that stand out:
* 2024-11-22 15:39:56 - A state transition that leads to changes that cause another state transition, did not actually trigger that transition.
* 2024-12-17 14:05:14 - Fixed race condition in forceSaveEntiteit.
* 2024-12-23 12:27:34 - Context removal is now prioritized above role removal; that is, when the user removes a contextrol that is filled and has the perspective to remove the context, it is done silently.
* 2024-12-23 15:50:13 - A new installation received all users. No longer so.
* 2025-01-06 12:18:07 - On entering a role state, when the state is of type Object state, and the user acquires a new perspective because of that object state, the peers should only be informed of the role that enters the state.
* 2025-01-06 15:44:01 - Final refinement for acquired perspectives based on state.
* 2025-01-10 15:41:02 - An error message is given if not all text in an ARC file can be parsed.
* 2025-01-21 09:29:15 - Added a form check on domain name in UpdateModel.
* 2025-03-25 12:31:45 - We now have a mechanism to report 'warnings' from the core. These are caught errors. The warnings will be reported through the error handler of the proxy call (if any).
* 2025-04-04 16:00:02 - Tooling for the modeller. We can now choose to save a compiled model to the Repository, just locally or not at all. Orthogonally, we can choose to apply a model to the current session immediately, or not. This gives us access to a number of good practices, such as: 

  * testing the syntax of a model (no store, don't apply in session) 
  * quick development (no store, apply in session), having a restart of the session as fallback in case of errors 
  * etc.
* 2025-04-07 15:30:19 - Compute users for the chat using the context of the ChatRoleInstance, not the type; in case the role is an Aspect role, we'll get the lexical context instead of the actual context.
*  2025-04-14 14:56:26 - The compiler now generates a warning if notify is used in a context without the Notification aspect. We can now restore a context from the GUI for another user.
* 2025-04-15 14:59:12 - Notifications can now be clicked to open the corresponding context.
* 2025-04-16 09:31:04 - We now have the property facet 'readableName'. This gives control over the names displayed for contexts and roles.
* 2025-04-24 12:17:23 - A first approach to document conflict solving for the Pouchdb connector.
* 2025-04-27 21:33:39 - A first approximation of a test to see whether we're offline. It is used to block fixing references for resources that can only be retrieved over the interne.
* 2025-04-29 09:20:18 - We now compact the database on each startup. Furthermore, we check after almost each view lookup whether it contains obsolete resources. If so, we delete and rebuild the view.
* 2025-05-04 13:54:45 - On storing a model just locally after a succesful parse, we now take the attachments from the local store instead of from the repository. The values should be equal and it protects us against changes in the shape of the domeinfile.
* 2025-05-06 21:52:14 - The accordions in 'where' now close automatically on navigating to another context.
* 2025-05-09 13:30:57 - The syntax for properties in screens has been changed. Instead of listing the properties that must be shown, we now accept exceptions: properties that are *excluded* from those listed by the perspective, verbs that are *excluded* for a property even though the perspective allows them. Importantly we can have as many verb-exclusion clauses as needed, allowing for fine control.
* 2025-05-12 08:52:42 - A new protocol to set up channels between processes. The main difference is that the SharedWorkerChannel now explicitly asks for the channelId on setting up. Previously, perspectives-pageworker and perspectives-sharedworker sent the channelId as soon as possible. This led to a race condition with constructing the SharedWorkerChannel. On Safari, this meant the system didn't start up. That problem has been solved.
* 2025-05-19 14:18:08 - We are now notified when we try to fill a seond role with the same filler.
* 2025-05-28 12:54:07 - We now use fuzzysort to match notifications and markdowns in the model file after an edit with existing translations.
* 2025-06-05 16:44:13 - We now remove tombstone docs from the post database on startup.
* 2025-06-06 17:02:26 - A loading splash page.
* 2025-06-07 12:11:14 - Version and build in MyContexts About.
* 2025-06-10 21:05:38 - We can now swipe (on mobile) to remove a card, if our perspective allows it.
* 2025-06-20 16:40:57 - Removed the dependency on the forked purescript-parsing. Instead, I copied and modified the IndentParser.

### Breaking Changes
While some changes would break an existing installation, we take care to provide automatic upgrade scripts. 
