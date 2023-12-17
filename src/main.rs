fn main() -> pbls::Result<()> {
    let (connection, io_threads) = lsp_server::Connection::stdio();
    pbls::run(connection, log::Level::Trace)?;
    io_threads.join()?;
    Ok(())
}
